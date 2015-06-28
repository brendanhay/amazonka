{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation updates the Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target. By default, a gateway
-- does not have CHAP enabled; however, for added security, you might use
-- it.
--
-- When you update CHAP credentials, all existing connections on the target
-- are closed and initiators must reconnect with the new credentials.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateChapCredentials.html>
module Network.AWS.StorageGateway.UpdateChapCredentials
    (
    -- * Request
      UpdateChapCredentials
    -- ** Request constructor
    , updateChapCredentials
    -- ** Request lenses
    , uccSecretToAuthenticateTarget
    , uccTargetARN
    , uccSecretToAuthenticateInitiator
    , uccInitiatorName

    -- * Response
    , UpdateChapCredentialsResponse
    -- ** Response constructor
    , updateChapCredentialsResponse
    -- ** Response lenses
    , uccrTargetARN
    , uccrInitiatorName
    , uccrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateChapCredentialsInput$InitiatorName
-- -   UpdateChapCredentialsInput$SecretToAuthenticateInitiator
-- -   UpdateChapCredentialsInput$SecretToAuthenticateTarget
-- -   UpdateChapCredentialsInput$TargetARN
--
-- /See:/ 'updateChapCredentials' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uccSecretToAuthenticateTarget'
--
-- * 'uccTargetARN'
--
-- * 'uccSecretToAuthenticateInitiator'
--
-- * 'uccInitiatorName'
data UpdateChapCredentials = UpdateChapCredentials'
    { _uccSecretToAuthenticateTarget    :: !(Maybe Text)
    , _uccTargetARN                     :: !Text
    , _uccSecretToAuthenticateInitiator :: !Text
    , _uccInitiatorName                 :: !Text
    } deriving (Eq,Read,Show)

-- | 'UpdateChapCredentials' smart constructor.
updateChapCredentials :: Text -> Text -> Text -> UpdateChapCredentials
updateChapCredentials pTargetARN pSecretToAuthenticateInitiator pInitiatorName =
    UpdateChapCredentials'
    { _uccSecretToAuthenticateTarget = Nothing
    , _uccTargetARN = pTargetARN
    , _uccSecretToAuthenticateInitiator = pSecretToAuthenticateInitiator
    , _uccInitiatorName = pInitiatorName
    }

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
uccSecretToAuthenticateTarget :: Lens' UpdateChapCredentials (Maybe Text)
uccSecretToAuthenticateTarget = lens _uccSecretToAuthenticateTarget (\ s a -> s{_uccSecretToAuthenticateTarget = a});

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return the TargetARN for
-- specified VolumeARN.
uccTargetARN :: Lens' UpdateChapCredentials Text
uccTargetARN = lens _uccTargetARN (\ s a -> s{_uccTargetARN = a});

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
uccSecretToAuthenticateInitiator :: Lens' UpdateChapCredentials Text
uccSecretToAuthenticateInitiator = lens _uccSecretToAuthenticateInitiator (\ s a -> s{_uccSecretToAuthenticateInitiator = a});

-- | The iSCSI initiator that connects to the target.
uccInitiatorName :: Lens' UpdateChapCredentials Text
uccInitiatorName = lens _uccInitiatorName (\ s a -> s{_uccInitiatorName = a});

instance AWSRequest UpdateChapCredentials where
        type Sv UpdateChapCredentials = StorageGateway
        type Rs UpdateChapCredentials =
             UpdateChapCredentialsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateChapCredentialsResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "InitiatorName") <*>
                     (pure s))

instance ToHeaders UpdateChapCredentials where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateChapCredentials" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateChapCredentials where
        toJSON UpdateChapCredentials'{..}
          = object
              ["SecretToAuthenticateTarget" .=
                 _uccSecretToAuthenticateTarget,
               "TargetARN" .= _uccTargetARN,
               "SecretToAuthenticateInitiator" .=
                 _uccSecretToAuthenticateInitiator,
               "InitiatorName" .= _uccInitiatorName]

instance ToPath UpdateChapCredentials where
        toPath = const "/"

instance ToQuery UpdateChapCredentials where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'updateChapCredentialsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uccrTargetARN'
--
-- * 'uccrInitiatorName'
--
-- * 'uccrStatus'
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
    { _uccrTargetARN     :: !(Maybe Text)
    , _uccrInitiatorName :: !(Maybe Text)
    , _uccrStatus        :: !Status
    } deriving (Eq,Read,Show)

-- | 'UpdateChapCredentialsResponse' smart constructor.
updateChapCredentialsResponse :: Status -> UpdateChapCredentialsResponse
updateChapCredentialsResponse pStatus =
    UpdateChapCredentialsResponse'
    { _uccrTargetARN = Nothing
    , _uccrInitiatorName = Nothing
    , _uccrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
uccrTargetARN :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrTargetARN = lens _uccrTargetARN (\ s a -> s{_uccrTargetARN = a});

-- | The iSCSI initiator that connects to the target. This is the same
-- initiator name specified in the request.
uccrInitiatorName :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrInitiatorName = lens _uccrInitiatorName (\ s a -> s{_uccrInitiatorName = a});

-- | FIXME: Undocumented member.
uccrStatus :: Lens' UpdateChapCredentialsResponse Status
uccrStatus = lens _uccrStatus (\ s a -> s{_uccrStatus = a});
