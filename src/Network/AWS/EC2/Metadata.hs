{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.EC2.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Metadata
    ( Metadata(..)
    , metadata
    , metadataByKey
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Internal
import           Network.Http.Client    hiding (get)
import qualified System.IO.Streams      as Streams

data Metadata
    = AmiId
    | AmiLaunchIndex
    | AmiManifestPath
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

instance Show Metadata where
    show = BS.unpack . toBS

instance ToByteString Metadata where
    toBS meta = case meta of
        AmiId                 -> "ami-id"
        AmiLaunchIndex        -> "ami-launch-index"
        AmiManifestPath       -> "ami-manifest-path"
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

metadata :: MonadIO m => Metadata -> m ByteString
metadata = metadataByKey . toBS

metadataByKey :: MonadIO m => ByteString -> m ByteString
metadataByKey key = get $ "http://169.254.169.254/latest/meta-data" <> key

--
-- Internal
--

get :: MonadIO m => ByteString -> m ByteString
get url = liftIO $
    bracket (establishConnection url) closeConnection $ \conn -> do
        rq <- buildRequest $ http GET url
        sendRequest conn rq emptyBody
        receiveResponse conn $ \_ inp -> fromMaybe "" <$> Streams.read inp
