{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials
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
module Network.AWS.StorageGateway.V2013_06_30.DeleteChapCredentials
    (
    -- * Request
      DeleteChapCredentials
    -- ** Request constructor
    , mkDeleteChapCredentialsInput
    -- ** Request lenses
    , dcciTargetARN
    , dcciInitiatorName

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response lenses
    , dccoTargetARN
    , dccoInitiatorName
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteChapCredentials' request.
mkDeleteChapCredentialsInput :: Text -- ^ 'dcciTargetARN'
                             -> Text -- ^ 'dcciInitiatorName'
                             -> DeleteChapCredentials
mkDeleteChapCredentialsInput p1 p2 = DeleteChapCredentials
    { _dcciTargetARN = p1
    , _dcciInitiatorName = p2
    }
{-# INLINE mkDeleteChapCredentialsInput #-}

data DeleteChapCredentials = DeleteChapCredentials
    { _dcciTargetARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use
      -- the DescribeStorediSCSIVolumes operation to return to retrieve
      -- the TargetARN for specified VolumeARN.
    , _dcciInitiatorName :: Text
      -- ^ The iSCSI initiator that connects to the target.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dcciTargetARN :: Lens' DeleteChapCredentials (Text)
dcciTargetARN = lens _dcciTargetARN (\s a -> s { _dcciTargetARN = a })
{-# INLINE dcciTargetARN #-}

-- | The iSCSI initiator that connects to the target.
dcciInitiatorName :: Lens' DeleteChapCredentials (Text)
dcciInitiatorName = lens _dcciInitiatorName (\s a -> s { _dcciInitiatorName = a })
{-# INLINE dcciInitiatorName #-}

instance ToPath DeleteChapCredentials

instance ToQuery DeleteChapCredentials

instance ToHeaders DeleteChapCredentials

instance ToJSON DeleteChapCredentials

data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccoTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the target.
    , _dccoInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the target.
dccoTargetARN :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccoTargetARN = lens _dccoTargetARN (\s a -> s { _dccoTargetARN = a })
{-# INLINE dccoTargetARN #-}

-- | The iSCSI initiator that connects to the target.
dccoInitiatorName :: Lens' DeleteChapCredentialsResponse (Maybe Text)
dccoInitiatorName = lens _dccoInitiatorName (\s a -> s { _dccoInitiatorName = a })
{-# INLINE dccoInitiatorName #-}

instance FromJSON DeleteChapCredentialsResponse

instance AWSRequest DeleteChapCredentials where
    type Sv DeleteChapCredentials = StorageGateway
    type Rs DeleteChapCredentials = DeleteChapCredentialsResponse

    request = get
    response _ = jsonResponse
