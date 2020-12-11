{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- In addition to updating user attributes, this API can also be used to mark phone and email as verified.
-- Calling this action requires developer credentials.
module Network.AWS.CognitoIdentityProvider.AdminUpdateUserAttributes
  ( -- * Creating a request
    AdminUpdateUserAttributes (..),
    mkAdminUpdateUserAttributes,

    -- ** Request lenses
    auuaClientMetadata,
    auuaUserPoolId,
    auuaUsername,
    auuaUserAttributes,

    -- * Destructuring the response
    AdminUpdateUserAttributesResponse (..),
    mkAdminUpdateUserAttributesResponse,

    -- ** Response lenses
    auuarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to update the user's attributes as an administrator.
--
-- /See:/ 'mkAdminUpdateUserAttributes' smart constructor.
data AdminUpdateUserAttributes = AdminUpdateUserAttributes'
  { clientMetadata ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Lude.Text)),
    userPoolId :: Lude.Text,
    username :: Lude.Sensitive Lude.Text,
    userAttributes :: [AttributeType]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateUserAttributes' with the minimum fields required to make a request.
--
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
-- * 'userPoolId' - The user pool ID for the user pool where you want to update user attributes.
-- * 'username' - The user name of the user for whom you want to update user attributes.
mkAdminUpdateUserAttributes ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'username'
  Lude.Sensitive Lude.Text ->
  AdminUpdateUserAttributes
mkAdminUpdateUserAttributes pUserPoolId_ pUsername_ =
  AdminUpdateUserAttributes'
    { clientMetadata = Lude.Nothing,
      userPoolId = pUserPoolId_,
      username = pUsername_,
      userAttributes = Lude.mempty
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the AdminUpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your AdminUpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaClientMetadata :: Lens.Lens' AdminUpdateUserAttributes (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
auuaClientMetadata = Lens.lens (clientMetadata :: AdminUpdateUserAttributes -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: AdminUpdateUserAttributes)
{-# DEPRECATED auuaClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | The user pool ID for the user pool where you want to update user attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUserPoolId :: Lens.Lens' AdminUpdateUserAttributes Lude.Text
auuaUserPoolId = Lens.lens (userPoolId :: AdminUpdateUserAttributes -> Lude.Text) (\s a -> s {userPoolId = a} :: AdminUpdateUserAttributes)
{-# DEPRECATED auuaUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The user name of the user for whom you want to update user attributes.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUsername :: Lens.Lens' AdminUpdateUserAttributes (Lude.Sensitive Lude.Text)
auuaUsername = Lens.lens (username :: AdminUpdateUserAttributes -> Lude.Sensitive Lude.Text) (\s a -> s {username = a} :: AdminUpdateUserAttributes)
{-# DEPRECATED auuaUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuaUserAttributes :: Lens.Lens' AdminUpdateUserAttributes [AttributeType]
auuaUserAttributes = Lens.lens (userAttributes :: AdminUpdateUserAttributes -> [AttributeType]) (\s a -> s {userAttributes = a} :: AdminUpdateUserAttributes)
{-# DEPRECATED auuaUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

instance Lude.AWSRequest AdminUpdateUserAttributes where
  type
    Rs AdminUpdateUserAttributes =
      AdminUpdateUserAttributesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AdminUpdateUserAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AdminUpdateUserAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AdminUpdateUserAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AdminUpdateUserAttributes where
  toJSON AdminUpdateUserAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("Username" Lude..= username),
            Lude.Just ("UserAttributes" Lude..= userAttributes)
          ]
      )

instance Lude.ToPath AdminUpdateUserAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery AdminUpdateUserAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to update user attributes as an administrator.
--
-- /See:/ 'mkAdminUpdateUserAttributesResponse' smart constructor.
newtype AdminUpdateUserAttributesResponse = AdminUpdateUserAttributesResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdminUpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAdminUpdateUserAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AdminUpdateUserAttributesResponse
mkAdminUpdateUserAttributesResponse pResponseStatus_ =
  AdminUpdateUserAttributesResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
auuarsResponseStatus :: Lens.Lens' AdminUpdateUserAttributesResponse Lude.Int
auuarsResponseStatus = Lens.lens (responseStatus :: AdminUpdateUserAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AdminUpdateUserAttributesResponse)
{-# DEPRECATED auuarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
