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
-- Module      : Network.AWS.SecretsManager.GetResourcePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document that's attached to the specified secret. The JSON request string input and response output are shown formatted with white space and line breaks for better readability. Submit your input as a single line JSON string.
--
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetResourcePolicy
--
--
--
-- __Related operations__
--
--     * To attach a resource policy to a secret, use 'PutResourcePolicy' .
--
--     * To delete the resource-based policy that's attached to a secret, use 'DeleteResourcePolicy' .
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
--
--
--
module Network.AWS.SecretsManager.GetResourcePolicy
    (
    -- * Creating a Request
      getResourcePolicy
    , GetResourcePolicy
    -- * Request Lenses
    , grpSecretId

    -- * Destructuring the Response
    , getResourcePolicyResponse
    , GetResourcePolicyResponse
    -- * Response Lenses
    , grprsResourcePolicy
    , grprsARN
    , grprsName
    , grprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'getResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { _grpSecretId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpSecretId' - Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
getResourcePolicy
    :: Text -- ^ 'grpSecretId'
    -> GetResourcePolicy
getResourcePolicy pSecretId_ = GetResourcePolicy' {_grpSecretId = pSecretId_}


-- | Specifies the secret that you want to retrieve the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
grpSecretId :: Lens' GetResourcePolicy Text
grpSecretId = lens _grpSecretId (\ s a -> s{_grpSecretId = a})

instance AWSRequest GetResourcePolicy where
        type Rs GetResourcePolicy = GetResourcePolicyResponse
        request = postJSON secretsManager
        response
          = receiveJSON
              (\ s h x ->
                 GetResourcePolicyResponse' <$>
                   (x .?> "ResourcePolicy") <*> (x .?> "ARN") <*>
                     (x .?> "Name")
                     <*> (pure (fromEnum s)))

instance Hashable GetResourcePolicy where

instance NFData GetResourcePolicy where

instance ToHeaders GetResourcePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.GetResourcePolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetResourcePolicy where
        toJSON GetResourcePolicy'{..}
          = object
              (catMaybes [Just ("SecretId" .= _grpSecretId)])

instance ToPath GetResourcePolicy where
        toPath = const "/"

instance ToQuery GetResourcePolicy where
        toQuery = const mempty

-- | /See:/ 'getResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { _grprsResourcePolicy :: !(Maybe Text)
  , _grprsARN            :: !(Maybe Text)
  , _grprsName           :: !(Maybe Text)
  , _grprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsResourcePolicy' - A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
--
-- * 'grprsARN' - The ARN of the secret that the resource-based policy was retrieved for.
--
-- * 'grprsName' - The friendly name of the secret that the resource-based policy was retrieved for.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getResourcePolicyResponse
    :: Int -- ^ 'grprsResponseStatus'
    -> GetResourcePolicyResponse
getResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { _grprsResourcePolicy = Nothing
    , _grprsARN = Nothing
    , _grprsName = Nothing
    , _grprsResponseStatus = pResponseStatus_
    }


-- | A JSON-formatted string that describes the permissions that are associated with the attached secret. These permissions are combined with any permissions that are associated with the user or role that attempts to access this secret. The combined permissions specify who can access the secret and what actions they can perform. For more information, see <http://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and Access Control for AWS Secrets Manager> in the /AWS Secrets Manager User Guide/ .
grprsResourcePolicy :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsResourcePolicy = lens _grprsResourcePolicy (\ s a -> s{_grprsResourcePolicy = a})

-- | The ARN of the secret that the resource-based policy was retrieved for.
grprsARN :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsARN = lens _grprsARN (\ s a -> s{_grprsARN = a})

-- | The friendly name of the secret that the resource-based policy was retrieved for.
grprsName :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsName = lens _grprsName (\ s a -> s{_grprsName = a})

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetResourcePolicyResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\ s a -> s{_grprsResponseStatus = a})

instance NFData GetResourcePolicyResponse where
