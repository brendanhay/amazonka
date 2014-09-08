{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials
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
-- for each target-initiator pair. Example Request The following example shows
-- a request that returns the CHAP credentials of an iSCSI target. POST /
-- HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com x-amz-Date:
-- 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 235 {
-- "ChapCredentials": { "TargetName": "iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeChapCredentials
    (
    -- * Request
      DescribeChapCredentials
    -- ** Request constructor
    , mkDescribeChapCredentials
    -- ** Request lenses
    , dcc1TargetARN

    -- * Response
    , DescribeChapCredentialsResponse
    -- ** Response constructor
    , mkDescribeChapCredentialsResponse
    -- ** Response lenses
    , dccrrChapCredentials
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing the Amazon Resource Name (ARN) of the iSCSI volume
-- target.
newtype DescribeChapCredentials = DescribeChapCredentials
    { _dcc1TargetARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeChapCredentials' request.
mkDescribeChapCredentials :: Text -- ^ 'dcc1TargetARN'
                          -> DescribeChapCredentials
mkDescribeChapCredentials p1 = DescribeChapCredentials
    { _dcc1TargetARN = p1
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dcc1TargetARN :: Lens' DescribeChapCredentials Text
dcc1TargetARN = lens _dcc1TargetARN (\s a -> s { _dcc1TargetARN = a })

instance ToPath DescribeChapCredentials

instance ToQuery DescribeChapCredentials

instance ToHeaders DescribeChapCredentials

instance ToJSON DescribeChapCredentials

-- | A JSON object containing a .
newtype DescribeChapCredentialsResponse = DescribeChapCredentialsResponse
    { _dccrrChapCredentials :: [ChapInfo]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeChapCredentialsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDescribeChapCredentialsResponse :: DescribeChapCredentialsResponse
mkDescribeChapCredentialsResponse = DescribeChapCredentialsResponse
    { _dccrrChapCredentials = mempty
    }

-- | An array of ChapInfo objects that represent CHAP credentials. Each object
-- in the array contains CHAP credential information for one target-initiator
-- pair. If no CHAP credentials are set, an empty array is returned. CHAP
-- credential information is provided in a JSON object with the following
-- fields: InitiatorName: The iSCSI initiator that connects to the target.
-- SecretToAuthenticateInitiator: The secret key that the initiator (e.g.
-- Windows client) must provide to participate in mutual CHAP with the target.
-- SecretToAuthenticateTarget: The secret key that the target must provide to
-- participate in mutual CHAP with the initiator (e.g. Windows client).
-- TargetARN: The Amazon Resource Name (ARN) of the storage volume.
dccrrChapCredentials :: Lens' DescribeChapCredentialsResponse [ChapInfo]
dccrrChapCredentials =
    lens _dccrrChapCredentials (\s a -> s { _dccrrChapCredentials = a })

instance FromJSON DescribeChapCredentialsResponse

instance AWSRequest DescribeChapCredentials where
    type Sv DescribeChapCredentials = StorageGateway
    type Rs DescribeChapCredentials = DescribeChapCredentialsResponse

    request = get
    response _ = jsonResponse
