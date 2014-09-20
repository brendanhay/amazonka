{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation deletes Challenge-Handshake Authentication Protocol (CHAP)
-- credentials for a specified iSCSI target and initiator pair. Example
-- Request The following example shows a request that deletes the CHAP
-- credentials for an iSCSI target myvolume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DeleteChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" } HTTP/1.1 200
-- OK x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 161 { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" }.
module Network.AWS.StorageGateway.DeleteChapCredentials
    (
    -- * Request
      DeleteChapCredentials
    -- ** Request constructor
    , deleteChapCredentials
    -- ** Request lenses
    , dccTargetARN
    , dccInitiatorName

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response constructor
    , deleteChapCredentialsResponse
    -- ** Response lenses
    , dccrTargetARN
    , dccrInitiatorName
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- DeleteChapCredentialsInput$InitiatorName
-- DeleteChapCredentialsInput$TargetARN.
data DeleteChapCredentials = DeleteChapCredentials
    { _dccTargetARN :: Text
    , _dccInitiatorName :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteChapCredentials' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Text@
--
-- * @InitiatorName ::@ @Text@
--
deleteChapCredentials :: Text -- ^ 'dccTargetARN'
                      -> Text -- ^ 'dccInitiatorName'
                      -> DeleteChapCredentials
deleteChapCredentials p1 p2 = DeleteChapCredentials
    { _dccTargetARN = p1
    , _dccInitiatorName = p2
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dccTargetARN :: Lens' DeleteChapCredentials Text
dccTargetARN = lens _dccTargetARN (\s a -> s { _dccTargetARN = a })

-- | The iSCSI initiator that connects to the target.
dccInitiatorName :: Lens' DeleteChapCredentials Text
dccInitiatorName =
    lens _dccInitiatorName (\s a -> s { _dccInitiatorName = a })

instance ToPath DeleteChapCredentials

instance ToQuery DeleteChapCredentials

instance ToHeaders DeleteChapCredentials

instance ToJSON DeleteChapCredentials

-- | A JSON object containing the following fields:.
data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccrTargetARN :: Maybe Text
    , _dccrInitiatorName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteChapCredentialsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Maybe Text@
--
-- * @InitiatorName ::@ @Maybe Text@
--
deleteChapCredentialsResponse :: DeleteChapCredentialsResponse
deleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccrTargetARN = Nothing
    , _dccrInitiatorName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the target.
dccrTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccrTargetARN = lens _dccrTargetARN (\s a -> s { _dccrTargetARN = a })

-- | The iSCSI initiator that connects to the target.
dccrInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccrInitiatorName =
    lens _dccrInitiatorName (\s a -> s { _dccrInitiatorName = a })

instance FromJSON DeleteChapCredentialsResponse

instance AWSRequest DeleteChapCredentials where
    type Sv DeleteChapCredentials = StorageGateway
    type Rs DeleteChapCredentials = DeleteChapCredentialsResponse

    request = get
    response _ = jsonResponse
