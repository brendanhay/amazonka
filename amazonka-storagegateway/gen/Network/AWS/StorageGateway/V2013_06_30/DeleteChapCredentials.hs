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
    , deleteChapCredentials
    -- ** Request lenses
    , dcciInitiatorName
    , dcciTargetARN

    -- * Response
    , DeleteChapCredentialsResponse
    -- ** Response lenses
    , dccoInitiatorName
    , dccoTargetARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteChapCredentials' request.
deleteChapCredentials :: Text -- ^ 'dcciInitiatorName'
                      -> Text -- ^ 'dcciTargetARN'
                      -> DeleteChapCredentials
deleteChapCredentials p1 p2 = DeleteChapCredentials
    { _dcciInitiatorName = p1
    , _dcciTargetARN = p2
    }

data DeleteChapCredentials = DeleteChapCredentials
    { _dcciInitiatorName :: Text
      -- ^ The iSCSI initiator that connects to the target.
    , _dcciTargetARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use
      -- the DescribeStorediSCSIVolumes operation to return to retrieve
      -- the TargetARN for specified VolumeARN.
    } deriving (Show, Generic)

-- | The iSCSI initiator that connects to the target.
dcciInitiatorName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteChapCredentials
    -> f DeleteChapCredentials
dcciInitiatorName f x =
    (\y -> x { _dcciInitiatorName = y })
       <$> f (_dcciInitiatorName x)
{-# INLINE dcciInitiatorName #-}

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
dcciTargetARN
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteChapCredentials
    -> f DeleteChapCredentials
dcciTargetARN f x =
    (\y -> x { _dcciTargetARN = y })
       <$> f (_dcciTargetARN x)
{-# INLINE dcciTargetARN #-}

instance ToPath DeleteChapCredentials

instance ToQuery DeleteChapCredentials

instance ToHeaders DeleteChapCredentials

instance ToJSON DeleteChapCredentials

data DeleteChapCredentialsResponse = DeleteChapCredentialsResponse
    { _dccoInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target.
    , _dccoTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the target.
    } deriving (Show, Generic)

-- | The iSCSI initiator that connects to the target.
dccoInitiatorName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteChapCredentialsResponse
    -> f DeleteChapCredentialsResponse
dccoInitiatorName f x =
    (\y -> x { _dccoInitiatorName = y })
       <$> f (_dccoInitiatorName x)
{-# INLINE dccoInitiatorName #-}

-- | The Amazon Resource Name (ARN) of the target.
dccoTargetARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteChapCredentialsResponse
    -> f DeleteChapCredentialsResponse
dccoTargetARN f x =
    (\y -> x { _dccoTargetARN = y })
       <$> f (_dccoTargetARN x)
{-# INLINE dccoTargetARN #-}

instance FromJSON DeleteChapCredentialsResponse

instance AWSRequest DeleteChapCredentials where
    type Sv DeleteChapCredentials = StorageGateway
    type Rs DeleteChapCredentials = DeleteChapCredentialsResponse

    request = get
    response _ = jsonResponse
