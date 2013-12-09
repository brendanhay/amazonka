{-# LANGUAGE FlexibleContexts  #-}
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
    , metadata
    , metadataByKey
    , isEC2
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad.Error         (throwError)
import           Control.Monad.IO.Class
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Monoid
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Network.HTTP.Conduit

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
    | SecurityCredentials
    | SecurityCredential ByteString
    | AvailabilityZone
    | UserData

metadata :: (Applicative m, MonadIO m)
         => Metadata
         -> EitherT AWSError m ByteString
metadata = metadataByKey . toPath

metadataByKey :: (Applicative m, MonadIO m)
              => ByteString
              -> EitherT AWSError m ByteString
metadataByKey p = do
    rs <- fmapLT Ex . syncIO $ simpleHttp url
    case strip '\n' $ LBS.toStrict rs of
        "" -> throwError "Failed to receive any data"
        bs -> return bs
  where
    url = BS.unpack $ "http://169.254.169.254/latest/meta-data/" <> p

isEC2 :: (Functor m, MonadIO m) => m Bool
isEC2 = fmap isRight . runEitherT . syncIO $
    simpleHttp "http://instance-data/latest"

toPath :: Metadata -> ByteString
toPath meta = case meta of
    AMIId                -> "ami-id"
    AMILaunchIndex       -> "ami-launch-index"
    AMIManifestPath      -> "ami-manifest-path"
    Hostname             -> "hostname"
    InstanceAction       -> "instance-action"
    InstanceId           -> "instance-id"
    InstanceType         -> "instance-type"
    KernelId             -> "kernel-id"
    LocalHostname        -> "local-hostname"
    LocalIPV4            -> "local-ipv4"
    Mac                  -> "mac"
    Profile              -> "profile"
    PublicHostname       -> "public-hostname"
    PublicIPV4           -> "public-ipv4"
    ReservationId        -> "reservation-id"
    SecurityCredentials  -> "iam/security-credentials/"
    SecurityCredential r -> "iam/security-credentials/" <> r
    AvailabilityZone     -> "placement/availability-zone"
    UserData             -> "user-data"
