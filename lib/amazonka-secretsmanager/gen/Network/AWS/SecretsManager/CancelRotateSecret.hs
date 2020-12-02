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
-- Module      : Network.AWS.SecretsManager.CancelRotateSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables automatic scheduled rotation and cancels the rotation of a secret if one is currently in progress.
--
--
-- To re-enable scheduled rotation, call 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. This will immediately rotate your secret and then enable the automatic schedule.
--
-- To successfully start a rotation, the staging label @AWSPENDING@ must be in one of the following states:
--
--     * Not be attached to any version at all
--
--     * Attached to the same version as the staging label @AWSCURRENT@
--
--
--
-- If the staging label @AWSPENDING@ is attached to a different version than the version with @AWSCURRENT@ then the attempt to rotate fails.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:CancelRotateSecret
--
--
--
-- __Related operations__
--
--     * To configure rotation for a secret or to manually trigger a rotation, use 'RotateSecret' .
--
--     * To get the rotation configuration details for a secret, use 'DescribeSecret' .
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
--
--     * To list all of the versions currently associated with a secret, use 'ListSecretVersionIds' .
--
--
--
module Network.AWS.SecretsManager.CancelRotateSecret
    (
    -- * Creating a Request
      cancelRotateSecret
    , CancelRotateSecret
    -- * Request Lenses
    , crsSecretId

    -- * Destructuring the Response
    , cancelRotateSecretResponse
    , CancelRotateSecretResponse
    -- * Response Lenses
    , crsrsVersionId
    , crsrsARN
    , crsrsName
    , crsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'cancelRotateSecret' smart constructor.
newtype CancelRotateSecret = CancelRotateSecret'
  { _crsSecretId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelRotateSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsSecretId' - Specifies the secret for which you want to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
cancelRotateSecret
    :: Text -- ^ 'crsSecretId'
    -> CancelRotateSecret
cancelRotateSecret pSecretId_ = CancelRotateSecret' {_crsSecretId = pSecretId_}


-- | Specifies the secret for which you want to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
crsSecretId :: Lens' CancelRotateSecret Text
crsSecretId = lens _crsSecretId (\ s a -> s{_crsSecretId = a})

instance AWSRequest CancelRotateSecret where
        type Rs CancelRotateSecret =
             CancelRotateSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 CancelRotateSecretResponse' <$>
                   (x .?> "VersionId") <*> (x .?> "ARN") <*>
                     (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable CancelRotateSecret where

instance NFData CancelRotateSecret where

instance ToHeaders CancelRotateSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.CancelRotateSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CancelRotateSecret where
        toJSON CancelRotateSecret'{..}
          = object
              (catMaybes [Just ("SecretId" .= _crsSecretId)])

instance ToPath CancelRotateSecret where
        toPath = const "/"

instance ToQuery CancelRotateSecret where
        toQuery = const mempty

-- | /See:/ 'cancelRotateSecretResponse' smart constructor.
data CancelRotateSecretResponse = CancelRotateSecretResponse'
  { _crsrsVersionId      :: !(Maybe Text)
  , _crsrsARN            :: !(Maybe Text)
  , _crsrsName           :: !(Maybe Text)
  , _crsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelRotateSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsrsVersionId' - The unique identifier of the version of the secret that was created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
--
-- * 'crsrsARN' - The ARN of the secret for which rotation was canceled.
--
-- * 'crsrsName' - The friendly name of the secret for which rotation was canceled.
--
-- * 'crsrsResponseStatus' - -- | The response status code.
cancelRotateSecretResponse
    :: Int -- ^ 'crsrsResponseStatus'
    -> CancelRotateSecretResponse
cancelRotateSecretResponse pResponseStatus_ =
  CancelRotateSecretResponse'
    { _crsrsVersionId = Nothing
    , _crsrsARN = Nothing
    , _crsrsName = Nothing
    , _crsrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier of the version of the secret that was created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
crsrsVersionId :: Lens' CancelRotateSecretResponse (Maybe Text)
crsrsVersionId = lens _crsrsVersionId (\ s a -> s{_crsrsVersionId = a})

-- | The ARN of the secret for which rotation was canceled.
crsrsARN :: Lens' CancelRotateSecretResponse (Maybe Text)
crsrsARN = lens _crsrsARN (\ s a -> s{_crsrsARN = a})

-- | The friendly name of the secret for which rotation was canceled.
crsrsName :: Lens' CancelRotateSecretResponse (Maybe Text)
crsrsName = lens _crsrsName (\ s a -> s{_crsrsName = a})

-- | -- | The response status code.
crsrsResponseStatus :: Lens' CancelRotateSecretResponse Int
crsrsResponseStatus = lens _crsrsResponseStatus (\ s a -> s{_crsrsResponseStatus = a})

instance NFData CancelRotateSecretResponse where
