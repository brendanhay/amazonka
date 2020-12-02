{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.ValidateResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the JSON text of the resource-based policy document attached to the specified secret. The JSON request string input and response output displays formatted code with white space and line breaks for better readability. Submit your input as a single line JSON string. A resource-based policy is optional.
module Network.AWS.SecretsManager.ValidateResourcePolicy
  ( -- * Creating a Request
    validateResourcePolicy,
    ValidateResourcePolicy,

    -- * Request Lenses
    vrpSecretId,
    vrpResourcePolicy,

    -- * Destructuring the Response
    validateResourcePolicyResponse,
    ValidateResourcePolicyResponse,

    -- * Response Lenses
    vrprsValidationErrors,
    vrprsPolicyValidationPassed,
    vrprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'validateResourcePolicy' smart constructor.
data ValidateResourcePolicy = ValidateResourcePolicy'
  { _vrpSecretId ::
      !(Maybe Text),
    _vrpResourcePolicy :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidateResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrpSecretId' - The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- * 'vrpResourcePolicy' - Identifies the Resource Policy attached to the secret.
validateResourcePolicy ::
  -- | 'vrpResourcePolicy'
  Text ->
  ValidateResourcePolicy
validateResourcePolicy pResourcePolicy_ =
  ValidateResourcePolicy'
    { _vrpSecretId = Nothing,
      _vrpResourcePolicy = pResourcePolicy_
    }

-- | The identifier for the secret that you want to validate a resource policy. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
vrpSecretId :: Lens' ValidateResourcePolicy (Maybe Text)
vrpSecretId = lens _vrpSecretId (\s a -> s {_vrpSecretId = a})

-- | Identifies the Resource Policy attached to the secret.
vrpResourcePolicy :: Lens' ValidateResourcePolicy Text
vrpResourcePolicy = lens _vrpResourcePolicy (\s a -> s {_vrpResourcePolicy = a})

instance AWSRequest ValidateResourcePolicy where
  type Rs ValidateResourcePolicy = ValidateResourcePolicyResponse
  request = postJSON secretsManager
  response =
    receiveJSON
      ( \s h x ->
          ValidateResourcePolicyResponse'
            <$> (x .?> "ValidationErrors" .!@ mempty)
            <*> (x .?> "PolicyValidationPassed")
            <*> (pure (fromEnum s))
      )

instance Hashable ValidateResourcePolicy

instance NFData ValidateResourcePolicy

instance ToHeaders ValidateResourcePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("secretsmanager.ValidateResourcePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ValidateResourcePolicy where
  toJSON ValidateResourcePolicy' {..} =
    object
      ( catMaybes
          [ ("SecretId" .=) <$> _vrpSecretId,
            Just ("ResourcePolicy" .= _vrpResourcePolicy)
          ]
      )

instance ToPath ValidateResourcePolicy where
  toPath = const "/"

instance ToQuery ValidateResourcePolicy where
  toQuery = const mempty

-- | /See:/ 'validateResourcePolicyResponse' smart constructor.
data ValidateResourcePolicyResponse = ValidateResourcePolicyResponse'
  { _vrprsValidationErrors ::
      !( Maybe
           [ValidationErrorsEntry]
       ),
    _vrprsPolicyValidationPassed ::
      !(Maybe Bool),
    _vrprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ValidateResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vrprsValidationErrors' - Returns an error message if your policy doesn't pass validatation.
--
-- * 'vrprsPolicyValidationPassed' - Returns a message stating that your Reource Policy passed validation.
--
-- * 'vrprsResponseStatus' - -- | The response status code.
validateResourcePolicyResponse ::
  -- | 'vrprsResponseStatus'
  Int ->
  ValidateResourcePolicyResponse
validateResourcePolicyResponse pResponseStatus_ =
  ValidateResourcePolicyResponse'
    { _vrprsValidationErrors = Nothing,
      _vrprsPolicyValidationPassed = Nothing,
      _vrprsResponseStatus = pResponseStatus_
    }

-- | Returns an error message if your policy doesn't pass validatation.
vrprsValidationErrors :: Lens' ValidateResourcePolicyResponse [ValidationErrorsEntry]
vrprsValidationErrors = lens _vrprsValidationErrors (\s a -> s {_vrprsValidationErrors = a}) . _Default . _Coerce

-- | Returns a message stating that your Reource Policy passed validation.
vrprsPolicyValidationPassed :: Lens' ValidateResourcePolicyResponse (Maybe Bool)
vrprsPolicyValidationPassed = lens _vrprsPolicyValidationPassed (\s a -> s {_vrprsPolicyValidationPassed = a})

-- | -- | The response status code.
vrprsResponseStatus :: Lens' ValidateResourcePolicyResponse Int
vrprsResponseStatus = lens _vrprsResponseStatus (\s a -> s {_vrprsResponseStatus = a})

instance NFData ValidateResourcePolicyResponse
