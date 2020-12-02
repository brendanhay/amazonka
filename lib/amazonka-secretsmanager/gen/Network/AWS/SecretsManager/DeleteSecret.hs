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
-- Module      : Network.AWS.SecretsManager.DeleteSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an entire secret and all of its versions. You can optionally include a recovery window during which you can restore the secret. If you don't specify a recovery window value, the operation defaults to 30 days. Secrets Manager attaches a @DeletionDate@ stamp to the secret that specifies the end of the recovery window. At the end of the recovery window, Secrets Manager deletes the secret permanently.
--
--
-- At any time before recovery window ends, you can use 'RestoreSecret' to remove the @DeletionDate@ and cancel the deletion of the secret.
--
-- You cannot access the encrypted secret information in any secret that is scheduled for deletion. If you need to access that information, you must cancel the deletion with 'RestoreSecret' and then retrieve the information.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DeleteSecret
--
--
--
-- __Related operations__
--
--     * To create a secret, use 'CreateSecret' .
--
--     * To cancel deletion of a version of a secret before the recovery window has expired, use 'RestoreSecret' .
--
--
--
module Network.AWS.SecretsManager.DeleteSecret
    (
    -- * Creating a Request
      deleteSecret
    , DeleteSecret
    -- * Request Lenses
    , dsRecoveryWindowInDays
    , dsSecretId

    -- * Destructuring the Response
    , deleteSecretResponse
    , DeleteSecretResponse
    -- * Response Lenses
    , dsrsARN
    , dsrsName
    , dsrsDeletionDate
    , dsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'deleteSecret' smart constructor.
data DeleteSecret = DeleteSecret'
  { _dsRecoveryWindowInDays :: !(Maybe Integer)
  , _dsSecretId             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsRecoveryWindowInDays' - (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. This value can range from 7 to 30 days. The default value is 30.
--
-- * 'dsSecretId' - Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
deleteSecret
    :: Text -- ^ 'dsSecretId'
    -> DeleteSecret
deleteSecret pSecretId_ =
  DeleteSecret' {_dsRecoveryWindowInDays = Nothing, _dsSecretId = pSecretId_}


-- | (Optional) Specifies the number of days that Secrets Manager waits before it can delete the secret. This value can range from 7 to 30 days. The default value is 30.
dsRecoveryWindowInDays :: Lens' DeleteSecret (Maybe Integer)
dsRecoveryWindowInDays = lens _dsRecoveryWindowInDays (\ s a -> s{_dsRecoveryWindowInDays = a})

-- | Specifies the secret that you want to delete. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
dsSecretId :: Lens' DeleteSecret Text
dsSecretId = lens _dsSecretId (\ s a -> s{_dsSecretId = a})

instance AWSRequest DeleteSecret where
        type Rs DeleteSecret = DeleteSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSecretResponse' <$>
                   (x .?> "ARN") <*> (x .?> "Name") <*>
                     (x .?> "DeletionDate")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteSecret where

instance NFData DeleteSecret where

instance ToHeaders DeleteSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.DeleteSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSecret where
        toJSON DeleteSecret'{..}
          = object
              (catMaybes
                 [("RecoveryWindowInDays" .=) <$>
                    _dsRecoveryWindowInDays,
                  Just ("SecretId" .= _dsSecretId)])

instance ToPath DeleteSecret where
        toPath = const "/"

instance ToQuery DeleteSecret where
        toQuery = const mempty

-- | /See:/ 'deleteSecretResponse' smart constructor.
data DeleteSecretResponse = DeleteSecretResponse'
  { _dsrsARN            :: !(Maybe Text)
  , _dsrsName           :: !(Maybe Text)
  , _dsrsDeletionDate   :: !(Maybe POSIX)
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsARN' - The ARN of the secret that is now scheduled for deletion.
--
-- * 'dsrsName' - The friendly name of the secret that is now scheduled for deletion.
--
-- * 'dsrsDeletionDate' - The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteSecretResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteSecretResponse
deleteSecretResponse pResponseStatus_ =
  DeleteSecretResponse'
    { _dsrsARN = Nothing
    , _dsrsName = Nothing
    , _dsrsDeletionDate = Nothing
    , _dsrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the secret that is now scheduled for deletion.
dsrsARN :: Lens' DeleteSecretResponse (Maybe Text)
dsrsARN = lens _dsrsARN (\ s a -> s{_dsrsARN = a})

-- | The friendly name of the secret that is now scheduled for deletion.
dsrsName :: Lens' DeleteSecretResponse (Maybe Text)
dsrsName = lens _dsrsName (\ s a -> s{_dsrsName = a})

-- | The date and time after which this secret can be deleted by Secrets Manager and can no longer be restored. This value is the date and time of the delete request plus the number of days specified in @RecoveryWindowInDays@ .
dsrsDeletionDate :: Lens' DeleteSecretResponse (Maybe UTCTime)
dsrsDeletionDate = lens _dsrsDeletionDate (\ s a -> s{_dsrsDeletionDate = a}) . mapping _Time

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSecretResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteSecretResponse where
