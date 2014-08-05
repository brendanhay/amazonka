{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation updates the Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target. By default, a gateway does
-- not have CHAP enabled; however, for added security, you might use it. When
-- you update CHAP credentials, all existing connections on the target are
-- closed and initiators must reconnect with the new credentials. Example
-- Request The following example shows a request that updates CHAP credentials
-- for an iSCSI target. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.UpdateChapCredentials { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "SecretToAuthenticateInitiator": "111111111111", "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com",
-- "SecretToAuthenticateTarget": "222222222222" } HTTP/1.1 200 OK
-- x-amzn-RequestId: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Date: Wed, 25 Apr 2012 12:00:02 GMT Content-type:
-- application/x-amz-json-1.1 Content-length: 161 { "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume",
-- "InitiatorName":
-- "iqn.1991-05.com.microsoft:computername.domain.example.com" }.
module Network.AWS.StorageGateway.V2013_06_30.UpdateChapCredentials where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateChapCredentials' request.
updateChapCredentials :: Text -- ^ '_ucciSecretToAuthenticateInitiator'
                      -> Text -- ^ '_ucciInitiatorName'
                      -> Text -- ^ '_ucciTargetARN'
                      -> UpdateChapCredentials
updateChapCredentials p1 p2 p3 = UpdateChapCredentials
    { _ucciSecretToAuthenticateInitiator = p1
    , _ucciInitiatorName = p2
    , _ucciTargetARN = p3
    , _ucciSecretToAuthenticateTarget = Nothing
    }

data UpdateChapCredentials = UpdateChapCredentials
    { _ucciSecretToAuthenticateInitiator :: Text
      -- ^ The secret key that the initiator (e.g. Windows client) must
      -- provide to participate in mutual CHAP with the target.
    , _ucciInitiatorName :: Text
      -- ^ The iSCSI initiator that connects to the target.
    , _ucciTargetARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the iSCSI volume target. Use
      -- the DescribeStorediSCSIVolumes operation to return to retrieve
      -- the TargetARN for specified VolumeARN.
    , _ucciSecretToAuthenticateTarget :: Maybe Text
      -- ^ The secret key that the target must provide to participate in
      -- mutual CHAP with the initiator (e.g. Windows client).
    } deriving (Show, Generic)

makeLenses ''UpdateChapCredentials

instance ToPath UpdateChapCredentials

instance ToQuery UpdateChapCredentials

instance ToHeaders UpdateChapCredentials

instance ToJSON UpdateChapCredentials

data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse
    { _uccoInitiatorName :: Maybe Text
      -- ^ The iSCSI initiator that connects to the target. This is the same
      -- initiator name specified in the request.
    , _uccoTargetARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the target. This is the same
      -- target specified in the request.
    } deriving (Show, Generic)

makeLenses ''UpdateChapCredentialsResponse

instance FromJSON UpdateChapCredentialsResponse

instance AWSRequest UpdateChapCredentials where
    type Sv UpdateChapCredentials = StorageGateway
    type Rs UpdateChapCredentials = UpdateChapCredentialsResponse

    request = get
    response _ = jsonResponse
