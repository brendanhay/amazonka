{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the Challenge-Handshake Authentication Protocol
-- (CHAP) credentials for a specified iSCSI target. By default, a gateway
-- does not have CHAP enabled; however, for added security, you might use
-- it.
--
-- When you update CHAP credentials, all existing connections on the target
-- are closed and initiators must reconnect with the new credentials.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateChapCredentials.html AWS API Reference> for UpdateChapCredentials.
module Network.AWS.StorageGateway.UpdateChapCredentials
    (
    -- * Creating a Request
      updateChapCredentials
    , UpdateChapCredentials
    -- * Request Lenses
    , uccSecretToAuthenticateTarget
    , uccTargetARN
    , uccSecretToAuthenticateInitiator
    , uccInitiatorName

    -- * Destructuring the Response
    , updateChapCredentialsResponse
    , UpdateChapCredentialsResponse
    -- * Response Lenses
    , uccrsTargetARN
    , uccrsInitiatorName
    , uccrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateChapCredentialsInput$InitiatorName
-- -   UpdateChapCredentialsInput$SecretToAuthenticateInitiator
-- -   UpdateChapCredentialsInput$SecretToAuthenticateTarget
-- -   UpdateChapCredentialsInput$TargetARN
--
-- /See:/ 'updateChapCredentials' smart constructor.
data UpdateChapCredentials = UpdateChapCredentials'
    { _uccSecretToAuthenticateTarget    :: !(Maybe Text)
    , _uccTargetARN                     :: !Text
    , _uccSecretToAuthenticateInitiator :: !Text
    , _uccInitiatorName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateChapCredentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccSecretToAuthenticateTarget'
--
-- * 'uccTargetARN'
--
-- * 'uccSecretToAuthenticateInitiator'
--
-- * 'uccInitiatorName'
updateChapCredentials
    :: Text -- ^ 'uccTargetARN'
    -> Text -- ^ 'uccSecretToAuthenticateInitiator'
    -> Text -- ^ 'uccInitiatorName'
    -> UpdateChapCredentials
updateChapCredentials pTargetARN_ pSecretToAuthenticateInitiator_ pInitiatorName_ =
    UpdateChapCredentials'
    { _uccSecretToAuthenticateTarget = Nothing
    , _uccTargetARN = pTargetARN_
    , _uccSecretToAuthenticateInitiator = pSecretToAuthenticateInitiator_
    , _uccInitiatorName = pInitiatorName_
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
        type Rs UpdateChapCredentials =
             UpdateChapCredentialsResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 UpdateChapCredentialsResponse' <$>
                   (x .?> "TargetARN") <*> (x .?> "InitiatorName") <*>
                     (pure (fromEnum s)))

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
              (catMaybes
                 [("SecretToAuthenticateTarget" .=) <$>
                    _uccSecretToAuthenticateTarget,
                  Just ("TargetARN" .= _uccTargetARN),
                  Just
                    ("SecretToAuthenticateInitiator" .=
                       _uccSecretToAuthenticateInitiator),
                  Just ("InitiatorName" .= _uccInitiatorName)])

instance ToPath UpdateChapCredentials where
        toPath = const "/"

instance ToQuery UpdateChapCredentials where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'updateChapCredentialsResponse' smart constructor.
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
    { _uccrsTargetARN     :: !(Maybe Text)
    , _uccrsInitiatorName :: !(Maybe Text)
    , _uccrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateChapCredentialsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccrsTargetARN'
--
-- * 'uccrsInitiatorName'
--
-- * 'uccrsStatus'
updateChapCredentialsResponse
    :: Int -- ^ 'uccrsStatus'
    -> UpdateChapCredentialsResponse
updateChapCredentialsResponse pStatus_ =
    UpdateChapCredentialsResponse'
    { _uccrsTargetARN = Nothing
    , _uccrsInitiatorName = Nothing
    , _uccrsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the target. This is the same target
-- specified in the request.
uccrsTargetARN :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrsTargetARN = lens _uccrsTargetARN (\ s a -> s{_uccrsTargetARN = a});

-- | The iSCSI initiator that connects to the target. This is the same
-- initiator name specified in the request.
uccrsInitiatorName :: Lens' UpdateChapCredentialsResponse (Maybe Text)
uccrsInitiatorName = lens _uccrsInitiatorName (\ s a -> s{_uccrsInitiatorName = a});

-- | The response status code.
uccrsStatus :: Lens' UpdateChapCredentialsResponse Int
uccrsStatus = lens _uccrsStatus (\ s a -> s{_uccrsStatus = a});
