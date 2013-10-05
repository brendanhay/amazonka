{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieve an EC2 instance's local metadata.
module Network.AWS.EC2.Metadata
    ( Metadata(..)
    , doesMetadataExist
    , metadata
    , metadataByKey
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Monoid
import           Network.AWS.Internal
import           Network.Http.Client    hiding (get)
import qualified System.IO.Streams      as Streams

data Metadata
    = AMIId
    | AMILaunchIndex
    | AMIManifestPath
    | Hostname
    | InstanceAction
    | InstanceId
    | InstanceType
    | KernelId
    | LocalHostname
    | LocalIPV4
    | Mac
    | Profile
    | PublicHostname
    | PublicIPV4
    | ReservationId
    | SecurityCredentials ByteString
    | AvailabilityZone

doesMetadataExist :: MonadIO m => m Bool
doesMetadataExist = eitherT failure return . syncIO $ bracket
     (establishConnection localhost)
     closeConnection
     success
   where
     success = return . const True
     failure = return . const False

metadata :: (Applicative m, MonadIO m)
         => Metadata
         -> EitherT Error m ByteString
metadata = metadataByKey . toPath

metadataByKey :: (Applicative m, MonadIO m)
              => ByteString
              -> EitherT Error m ByteString
metadataByKey = get . mappend "/latest/meta-data/"

--
-- Internal
--

localhost :: ByteString
localhost = "http://169.254.169.254"

get :: (Applicative m, MonadIO m)
    => ByteString
    -> EitherT Error m ByteString
get url = do
    rs <- fmapLT Ex $ syncIO req
    sStripChar '\n' <$> rs ?? "Failed to receive any data"
  where
    req = bracket (establishConnection localhost) closeConnection $ \c -> do
        rq <- buildRequest $ http GET url
        sendRequest c rq emptyBody
        receiveResponse c $ const Streams.read

toPath :: Metadata -> ByteString
toPath meta = case meta of
    AMIId                 -> "ami-id"
    AMILaunchIndex        -> "ami-launch-index"
    AMIManifestPath       -> "ami-manifest-path"
    Hostname              -> "hostname"
    InstanceAction        -> "instance-action"
    InstanceId            -> "instance-id"
    InstanceType          -> "instance-type"
    KernelId              -> "kernel-id"
    LocalHostname         -> "local-hostname"
    LocalIPV4             -> "local-ipv4"
    Mac                   -> "mac"
    Profile               -> "profile"
    PublicHostname        -> "public-hostname"
    PublicIPV4            -> "public-ipv4"
    ReservationId         -> "reservation-id"
    SecurityCredentials r -> "iam/security-credentials/" <> r
    AvailabilityZone      -> "placement/availability-zone"
