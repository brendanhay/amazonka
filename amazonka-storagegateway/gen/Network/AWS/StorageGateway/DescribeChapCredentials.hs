{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns an array of Challenge-Handshake Authentication
-- Protocol (CHAP) credentials information for a specified iSCSI target, one
-- for each target-initiator pair.
module Network.AWS.StorageGateway.DescribeChapCredentials
    (
    -- * Request
      DescribeChapCredentials
    -- ** Request constructor
    , describeChapCredentials
    -- ** Request lenses
    , dccTargetARN

    -- * Response
    , DescribeChapCredentialsResponse
    -- ** Response constructor
    , describeChapCredentialsResponse
    -- ** Response lenses
    , dccrChapCredentials
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DescribeChapCredentials = DescribeChapCredentials
    { _dccTargetARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeChapCredentials' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccTargetARN' @::@ 'Text'
--
describeChapCredentials :: Text -- ^ 'dccTargetARN'
                        -> DescribeChapCredentials
describeChapCredentials p1 = DescribeChapCredentials
    { _dccTargetARN = p1
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dccTargetARN :: Lens' DescribeChapCredentials Text
dccTargetARN = lens _dccTargetARN (\s a -> s { _dccTargetARN = a })

newtype DescribeChapCredentialsResponse = DescribeChapCredentialsResponse
    { _dccrChapCredentials :: [ChapInfo]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeChapCredentialsResponse where
    type Item DescribeChapCredentialsResponse = ChapInfo

    fromList = DescribeChapCredentialsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dccrChapCredentials

-- | 'DescribeChapCredentialsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dccrChapCredentials' @::@ ['ChapInfo']
--
describeChapCredentialsResponse :: DescribeChapCredentialsResponse
describeChapCredentialsResponse = DescribeChapCredentialsResponse
    { _dccrChapCredentials = mempty
    }

-- | An array of ChapInfo objects that represent CHAP credentials. Each object
-- in the array contains CHAP credential information for one
-- target-initiator pair. If no CHAP credentials are set, an empty array is
-- returned. CHAP credential information is provided in a JSON object with
-- the following fields: InitiatorName: The iSCSI initiator that connects to
-- the target. SecretToAuthenticateInitiator: The secret key that the
-- initiator (e.g. Windows client) must provide to participate in mutual
-- CHAP with the target. SecretToAuthenticateTarget: The secret key that the
-- target must provide to participate in mutual CHAP with the initiator
-- (e.g. Windows client). TargetARN: The Amazon Resource Name (ARN) of the
-- storage volume.
dccrChapCredentials :: Lens' DescribeChapCredentialsResponse [ChapInfo]
dccrChapCredentials =
    lens _dccrChapCredentials (\s a -> s { _dccrChapCredentials = a })

instance AWSRequest DescribeChapCredentials where
    type Sv DescribeChapCredentials = StorageGateway
    type Rs DescribeChapCredentials = DescribeChapCredentialsResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeChapCredentialsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DescribeChapCredentials where
    toPath = const "/"

instance ToHeaders DescribeChapCredentials

instance ToQuery DescribeChapCredentials where
    toQuery = const mempty

instance ToJSON DescribeChapCredentials where
    toJSON = genericToJSON jsonOptions
