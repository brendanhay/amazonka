{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateChapCredentials
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateChapCredentials.html>
module Network.AWS.StorageGateway.UpdateChapCredentials
    (
    -- * Request
      UpdateChapCredentials
    -- ** Request constructor
    , updateChapCredentials
    -- ** Request lenses
    , uccrqSecretToAuthenticateTarget
    , uccrqTargetARN
    , uccrqSecretToAuthenticateInitiator
    , uccrqInitiatorName

    -- * Response
    , UpdateChapCredentialsResponse
    -- ** Response constructor
    , updateChapCredentialsResponse
    -- ** Response lenses
    , uccrsTargetARN
    , uccrsInitiatorName
    , uccrsStatus
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
-- * 'uccrqSecretToAuthenticateTarget'
--
-- * 'uccrqTargetARN'
--
-- * 'uccrqSecretToAuthenticateInitiator'
--
-- * 'uccrqInitiatorName'
data UpdateChapCredentials = UpdateChapCredentials'
    { _uccrqSecretToAuthenticateTarget    :: !(Maybe Text)
    , _uccrqTargetARN                     :: !Text
    , _uccrqSecretToAuthenticateInitiator :: !Text
    , _uccrqInitiatorName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateChapCredentials' smart constructor.
updateChapCredentials :: Text -> Text -> Text -> UpdateChapCredentials
updateChapCredentials pTargetARN_ pSecretToAuthenticateInitiator_ pInitiatorName_ =
    UpdateChapCredentials'
    { _uccrqSecretToAuthenticateTarget = Nothing
    , _uccrqTargetARN = pTargetARN_
    , _uccrqSecretToAuthenticateInitiator = pSecretToAuthenticateInitiator_
    , _uccrqInitiatorName = pInitiatorName_
    }

-- | The secret key that the target must provide to participate in mutual
-- CHAP with the initiator (e.g. Windows client).
--
-- Byte constraints: Minimum bytes of 12. Maximum bytes of 16.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
uccrqSecretToAuthenticateTarget :: Lens' UpdateChapCredentials (Maybe Text)
uccrqSecretToAuthenticateTarget = lens _uccrqSecretToAuthenticateTarget (\ s a -> s{_uccrqSecretToAuthenticateTarget = a});

-- | The Amazon Resource Name (ARN) of the iSCSI volume target. Use the
-- DescribeStorediSCSIVolumes operation to return the TargetARN for
-- specified VolumeARN.
uccrqTargetARN :: Lens' UpdateChapCredentials Text
uccrqTargetARN = lens _uccrqTargetARN (\ s a -> s{_uccrqTargetARN = a});

-- | The secret key that the initiator (for example, the Windows client) must
-- provide to participate in mutual CHAP with the target.
--
-- The secret key must be between 12 and 16 bytes when encoded in UTF-8.
uccrqSecretToAuthenticateInitiator :: Lens' UpdateChapCredentials Text
uccrqSecretToAuthenticateInitiator = lens _uccrqSecretToAuthenticateInitiator (\ s a -> s{_uccrqSecretToAuthenticateInitiator = a});

-- | The iSCSI initiator that connects to the target.
uccrqInitiatorName :: Lens' UpdateChapCredentials Text
uccrqInitiatorName = lens _uccrqInitiatorName (\ s a -> s{_uccrqInitiatorName = a});

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
              ["SecretToAuthenticateTarget" .=
                 _uccrqSecretToAuthenticateTarget,
               "TargetARN" .= _uccrqTargetARN,
               "SecretToAuthenticateInitiator" .=
                 _uccrqSecretToAuthenticateInitiator,
               "InitiatorName" .= _uccrqInitiatorName]

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
-- * 'uccrsTargetARN'
--
-- * 'uccrsInitiatorName'
--
-- * 'uccrsStatus'
data UpdateChapCredentialsResponse = UpdateChapCredentialsResponse'
    { _uccrsTargetARN     :: !(Maybe Text)
    , _uccrsInitiatorName :: !(Maybe Text)
    , _uccrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateChapCredentialsResponse' smart constructor.
updateChapCredentialsResponse :: Int -> UpdateChapCredentialsResponse
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

-- | FIXME: Undocumented member.
uccrsStatus :: Lens' UpdateChapCredentialsResponse Int
uccrsStatus = lens _uccrsStatus (\ s a -> s{_uccrsStatus = a});
