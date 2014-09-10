{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway
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
module Network.AWS.StorageGateway
    (
    -- * Request
      UpdateChapCredentials
    -- ** Request constructor
    , mkUpdateChapCredentials
    -- ** Request lenses
    , uccTargetARN
    , uccSecretToAuthenticateInitiator
    , uccInitiatorName
    , uccSecretToAuthenticateTarget

    -- * Response
    , UpdateChapCredentialsResponse
    -- ** Response constructor
    , mkUpdateChapCredentialsResponse
    -- ** Response lenses
    , uccrTargetARN
    , uccrInitiatorName
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | A JSON object containing one or more of the following fields:
-- UpdateChapCredentialsInput$InitiatorName
-- UpdateChapCredentialsInput$SecretToAuthenticateInitiator
-- UpdateChapCredentialsInput$SecretToAuthenticateTarget
-- UpdateChapCredentialsInput$TargetARN.
data UpdateChapCredentials = UpdateChapCredentials
    { _uccTargetARN :: !Text
    , _uccSecretToAuthenticateInitiator :: !Text
    , _uccInitiatorName :: !Text
    , _uccSecretToAuthenticateTarget :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateChapCredentials' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Text@
--
-- * @SecretToAuthenticateInitiator ::@ @Text@
--
-- * @InitiatorName ::@ @Text@
--
-- * @SecretToAuthenticateTarget ::@ @Maybe Text@
--
mkUpdateChapCredentials :: Text -- ^ 'uccTargetARN'
                        -> Text -- ^ 'uccSecretToAuthenticateInitiator'
                        -> Text -- ^ 'uccInitiatorName'
                        -> UpdateChapCredentials
mkUpdateChapCredentials p1 p2 p3 = UpdateChapCredentials
    { _uccTargetARN = p1
    , _uccSecretToAuthenticateInitiator = p2
    , _uccInitiatorName = p3
    , _uccSecretToAuthenticateTarget = Nothing
    }

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return to retrieve the TargetARN
-- for specified VolumeARN.
uccTargetARN :: Lens' UpdateChapCredentials Text
uccTargetARN = lens _uccTargetARN (\s a -> s { _uccTargetARN = a })

-- | The secret key that the initiator (e.g. Windows client) must provide to
-- participate in mutual CHAP with the target.
uccSecretToAuthenticateInitiator :: Lens' UpdateChapCredentials Text
uccSecretToAuthenticateInitiator =
    lens _uccSecretToAuthenticateInitiator
         (\s a -> s { _uccSecretToAuthenticateInitiator = a })

-- | The iSCSI initiator that connects to the target.
uccInitiatorName :: Lens' UpdateChapCredentials Text
uccInitiatorName =
    lens _uccInitiatorName (\s a -> s { _uccInitiatorName = a })

-- | The secret key that the target must provide to participate in mutual CHAP
-- with the initiator (e.g. Windows client).
uccSecretToAuthenticateTarget :: Lens' UpdateChapCredentials (Maybe Text)
uccSecretToAuthenticateTarget =
    lens _uccSecretToAuthenticateTarget
         (\s a -> s { _uccSecretToAuthenticateTarget = a })

instance ToPath UpdateChapCredentials

instance ToQuery UpdateChapCredentials

instance ToHeaders UpdateChapCredentials

instance ToJSON UpdateChapCredentials

-- | A JSON object containing the following fields:.
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse
    { _uccrTargetARN :: !(Maybe Text)
    , _uccrInitiatorName :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateChapCredentialsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TargetARN ::@ @Maybe Text@
--
-- * @InitiatorName ::@ @Maybe Text@
--
mkUpdateChapCredentialsResponse :: UpdateChapCredentialsResponse
mkUpdateChapCredentialsResponse = UpdateChapCredentialsResponse
    { _uccrTargetARN = Nothing
    , _uccrInitiatorName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
uccrTargetARN :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrTargetARN = lens _uccrTargetARN (\s a -> s { _uccrTargetARN = a })

-- | The iSCSI initiator that connects to the target. This is the same initiator
-- name specified in the request.
uccrInitiatorName :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrInitiatorName =
    lens _uccrInitiatorName (\s a -> s { _uccrInitiatorName = a })

instance FromJSON UpdateChapCredentialsResponse

instance AWSRequest UpdateChapCredentials where
    type Sv UpdateChapCredentials = StorageGateway
    type Rs UpdateChapCredentials = UpdateChapCredentialsResponse

    request = get
    response _ = jsonResponse
