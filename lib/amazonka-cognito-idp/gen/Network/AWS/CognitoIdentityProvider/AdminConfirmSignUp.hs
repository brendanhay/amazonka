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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms user registration as an admin without using a confirmation code. Works on any user.
--
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminConfirmSignUp
  ( -- * Creating a Request
    adminConfirmSignUp,
    AdminConfirmSignUp,

    -- * Request Lenses
    acsuClientMetadata,
    acsuUserPoolId,
    acsuUsername,

    -- * Destructuring the Response
    adminConfirmSignUpResponse,
    AdminConfirmSignUpResponse,

    -- * Response Lenses
    acsursResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to confirm user registration.
--
--
--
-- /See:/ 'adminConfirmSignUp' smart constructor.
data AdminConfirmSignUp = AdminConfirmSignUp'
  { _acsuClientMetadata ::
      !(Maybe (Map Text (Text))),
    _acsuUserPoolId :: !Text,
    _acsuUsername :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminConfirmSignUp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsuClientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- * 'acsuUserPoolId' - The user pool ID for which you want to confirm user registration.
--
-- * 'acsuUsername' - The user name for which you want to confirm user registration.
adminConfirmSignUp ::
  -- | 'acsuUserPoolId'
  Text ->
  -- | 'acsuUsername'
  Text ->
  AdminConfirmSignUp
adminConfirmSignUp pUserPoolId_ pUsername_ =
  AdminConfirmSignUp'
    { _acsuClientMetadata = Nothing,
      _acsuUserPoolId = pUserPoolId_,
      _acsuUsername = _Sensitive # pUsername_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  If your user pool configuration includes triggers, the AdminConfirmSignUp API action invokes the AWS Lambda function that is specified for the /post confirmation/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. In this payload, the @clientMetadata@ attribute provides the data that you assigned to the ClientMetadata parameter in your AdminConfirmSignUp request. In your function code in AWS Lambda, you can process the ClientMetadata value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
acsuClientMetadata :: Lens' AdminConfirmSignUp (HashMap Text (Text))
acsuClientMetadata = lens _acsuClientMetadata (\s a -> s {_acsuClientMetadata = a}) . _Default . _Map

-- | The user pool ID for which you want to confirm user registration.
acsuUserPoolId :: Lens' AdminConfirmSignUp Text
acsuUserPoolId = lens _acsuUserPoolId (\s a -> s {_acsuUserPoolId = a})

-- | The user name for which you want to confirm user registration.
acsuUsername :: Lens' AdminConfirmSignUp Text
acsuUsername = lens _acsuUsername (\s a -> s {_acsuUsername = a}) . _Sensitive

instance AWSRequest AdminConfirmSignUp where
  type Rs AdminConfirmSignUp = AdminConfirmSignUpResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveEmpty
      (\s h x -> AdminConfirmSignUpResponse' <$> (pure (fromEnum s)))

instance Hashable AdminConfirmSignUp

instance NFData AdminConfirmSignUp

instance ToHeaders AdminConfirmSignUp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.AdminConfirmSignUp" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AdminConfirmSignUp where
  toJSON AdminConfirmSignUp' {..} =
    object
      ( catMaybes
          [ ("ClientMetadata" .=) <$> _acsuClientMetadata,
            Just ("UserPoolId" .= _acsuUserPoolId),
            Just ("Username" .= _acsuUsername)
          ]
      )

instance ToPath AdminConfirmSignUp where
  toPath = const "/"

instance ToQuery AdminConfirmSignUp where
  toQuery = const mempty

-- | Represents the response from the server for the request to confirm registration.
--
--
--
-- /See:/ 'adminConfirmSignUpResponse' smart constructor.
newtype AdminConfirmSignUpResponse = AdminConfirmSignUpResponse'
  { _acsursResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminConfirmSignUpResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acsursResponseStatus' - -- | The response status code.
adminConfirmSignUpResponse ::
  -- | 'acsursResponseStatus'
  Int ->
  AdminConfirmSignUpResponse
adminConfirmSignUpResponse pResponseStatus_ =
  AdminConfirmSignUpResponse'
    { _acsursResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
acsursResponseStatus :: Lens' AdminConfirmSignUpResponse Int
acsursResponseStatus = lens _acsursResponseStatus (\s a -> s {_acsursResponseStatus = a})

instance NFData AdminConfirmSignUpResponse
