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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user's attributes, including developer attributes, as an administrator. Works on any user.
--
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- In addition to updating user attributes, this API can also be used to mark phone and email as verified.
--
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
  ( -- * Creating a Request
    adminUpdateUserAttributes,
    AdminUpdateUserAttributes,

    -- * Request Lenses
    auuaClientMetadata,
    auuaUserPoolId,
    auuaUsername,
    auuaUserAttributes,

    -- * Destructuring the Response
    adminUpdateUserAttributesResponse,
    AdminUpdateUserAttributesResponse,

    -- * Response Lenses
    auuarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update the user's attributes as an administrator.
--
--
--
-- /See:/ 'adminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { _auuaClientMetadata ::
      !(Maybe (Map Text (Text))),
    _auuaUserPoolId :: !Text,
    _auuaUsername :: !(Sensitive Text),
    _auuaUserAttributes :: ![AttributeType]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminUpdateUserAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auuaClientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- * 'auuaUserPoolId' - The user pool ID for the user pool where you want to update user attributes.
--
-- * 'auuaUsername' - The user name of the user for whom you want to update user attributes.
--
-- * 'auuaUserAttributes' - An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
adminUpdateUserAttributes ::
  -- | 'auuaUserPoolId'
  Text ->
  -- | 'auuaUsername'
  Text ->
  AdminUpdateUserAttributes
adminUpdateUserAttributes pUserPoolId_ pUsername_ =
  AdminUpdateUserAttributes'
    { _auuaClientMetadata = Nothing,
      _auuaUserPoolId = pUserPoolId_,
      _auuaUsername = _Sensitive # pUsername_,
      _auuaUserAttributes = mempty
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.  You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
auuaClientMetadata :: Lens' AdminUpdateUserAttributes (HashMap Text (Text))
auuaClientMetadata = lens _auuaClientMetadata (\s a -> s {_auuaClientMetadata = a}) . _Default . _Map

-- | The user pool ID for the user pool where you want to update user attributes.
auuaUserPoolId :: Lens' AdminUpdateUserAttributes Text
auuaUserPoolId = lens _auuaUserPoolId (\s a -> s {_auuaUserPoolId = a})

-- | The user name of the user for whom you want to update user attributes.
auuaUsername :: Lens' AdminUpdateUserAttributes Text
auuaUsername = lens _auuaUsername (\s a -> s {_auuaUsername = a}) . _Sensitive

-- | An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
auuaUserAttributes :: Lens' AdminUpdateUserAttributes [AttributeType]
auuaUserAttributes = lens _auuaUserAttributes (\s a -> s {_auuaUserAttributes = a}) . _Coerce

instance AWSRequest AdminUpdateUserAttributes where
  type
    Rs AdminUpdateUserAttributes =
      AdminUpdateUserAttributesResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveEmpty
      ( \s h x ->
          AdminUpdateUserAttributesResponse' <$> (pure (fromEnum s))
      )

instance Hashable AdminUpdateUserAttributes

instance NFData AdminUpdateUserAttributes

instance ToHeaders AdminUpdateUserAttributes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.AdminUpdateUserAttributes" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AdminUpdateUserAttributes where
  toJSON AdminUpdateUserAttributes' {..} =
    object
      ( catMaybes
          [ ("ClientMetadata" .=) <$> _auuaClientMetadata,
            Just ("UserPoolId" .= _auuaUserPoolId),
            Just ("Username" .= _auuaUsername),
            Just ("UserAttributes" .= _auuaUserAttributes)
          ]
      )

instance ToPath AdminUpdateUserAttributes where
  toPath = const "/"

instance ToQuery AdminUpdateUserAttributes where
  toQuery = const mempty

-- | Represents the response from the server for the request to update user attributes as an administrator.
--
--
--
-- /See:/ 'adminUpdateUserAttributesResponse' smart constructor.
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { _auuarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminUpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'auuarsResponseStatus' - -- | The response status code.
adminUpdateUserAttributesResponse ::
  -- | 'auuarsResponseStatus'
  Int ->
  AdminUpdateUserAttributesResponse
adminUpdateUserAttributesResponse pResponseStatus_ =
  AdminUpdateUserAttributesResponse'
    { _auuarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
auuarsResponseStatus :: Lens' AdminUpdateUserAttributesResponse Int
auuarsResponseStatus = lens _auuarsResponseStatus (\s a -> s {_auuarsResponseStatus = a})

instance NFData AdminUpdateUserAttributesResponse
