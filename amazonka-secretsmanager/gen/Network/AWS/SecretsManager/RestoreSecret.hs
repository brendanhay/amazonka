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
-- Module      : Network.AWS.SecretsManager.RestoreSecret
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the scheduled deletion of a secret by removing the @DeletedDate@ time stamp. This makes the secret accessible to query once again.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RestoreSecret
--
--
--
-- __Related operations__
--
--     * To delete a secret, use 'DeleteSecret' .
--
--
--
module Network.AWS.SecretsManager.RestoreSecret
    (
    -- * Creating a Request
      restoreSecret
    , RestoreSecret
    -- * Request Lenses
    , rSecretId

    -- * Destructuring the Response
    , restoreSecretResponse
    , RestoreSecretResponse
    -- * Response Lenses
    , rrsARN
    , rrsName
    , rrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'restoreSecret' smart constructor.
newtype RestoreSecret = RestoreSecret'
  { _rSecretId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreSecret' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rSecretId' - Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
restoreSecret
    :: Text -- ^ 'rSecretId'
    -> RestoreSecret
restoreSecret pSecretId_ = RestoreSecret' {_rSecretId = pSecretId_}


-- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
rSecretId :: Lens' RestoreSecret Text
rSecretId = lens _rSecretId (\ s a -> s{_rSecretId = a})

instance AWSRequest RestoreSecret where
        type Rs RestoreSecret = RestoreSecretResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 RestoreSecretResponse' <$>
                   (x .?> "ARN") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable RestoreSecret where

instance NFData RestoreSecret where

instance ToHeaders RestoreSecret where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.RestoreSecret" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RestoreSecret where
        toJSON RestoreSecret'{..}
          = object
              (catMaybes [Just ("SecretId" .= _rSecretId)])

instance ToPath RestoreSecret where
        toPath = const "/"

instance ToQuery RestoreSecret where
        toQuery = const mempty

-- | /See:/ 'restoreSecretResponse' smart constructor.
data RestoreSecretResponse = RestoreSecretResponse'
  { _rrsARN            :: !(Maybe Text)
  , _rrsName           :: !(Maybe Text)
  , _rrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreSecretResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsARN' - The ARN of the secret that was restored.
--
-- * 'rrsName' - The friendly name of the secret that was restored.
--
-- * 'rrsResponseStatus' - -- | The response status code.
restoreSecretResponse
    :: Int -- ^ 'rrsResponseStatus'
    -> RestoreSecretResponse
restoreSecretResponse pResponseStatus_ =
  RestoreSecretResponse'
    { _rrsARN = Nothing
    , _rrsName = Nothing
    , _rrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the secret that was restored.
rrsARN :: Lens' RestoreSecretResponse (Maybe Text)
rrsARN = lens _rrsARN (\ s a -> s{_rrsARN = a})

-- | The friendly name of the secret that was restored.
rrsName :: Lens' RestoreSecretResponse (Maybe Text)
rrsName = lens _rrsName (\ s a -> s{_rrsName = a})

-- | -- | The response status code.
rrsResponseStatus :: Lens' RestoreSecretResponse Int
rrsResponseStatus = lens _rrsResponseStatus (\ s a -> s{_rrsResponseStatus = a})

instance NFData RestoreSecretResponse where
