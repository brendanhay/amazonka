{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to update a specific attribute (one at a time).
module Network.AWS.CognitoIdentityProvider.UpdateUserAttributes
  ( -- * Creating a request
    UpdateUserAttributes (..),
    mkUpdateUserAttributes,

    -- ** Request lenses
    uuaClientMetadata,
    uuaUserAttributes,
    uuaAccessToken,

    -- * Destructuring the response
    UpdateUserAttributesResponse (..),
    mkUpdateUserAttributesResponse,

    -- ** Response lenses
    uuarsCodeDeliveryDetailsList,
    uuarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to update user attributes.
--
-- /See:/ 'mkUpdateUserAttributes' smart constructor.
data UpdateUserAttributes = UpdateUserAttributes'
  { clientMetadata ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    userAttributes :: [AttributeType],
    accessToken :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserAttributes' with the minimum fields required to make a request.
--
-- * 'accessToken' - The access token for the request to update user attributes.
-- * 'clientMetadata' - A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the UpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your UpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
-- * 'userAttributes' - An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
mkUpdateUserAttributes ::
  -- | 'accessToken'
  Lude.Sensitive Lude.Text ->
  UpdateUserAttributes
mkUpdateUserAttributes pAccessToken_ =
  UpdateUserAttributes'
    { clientMetadata = Lude.Nothing,
      userAttributes = Lude.mempty,
      accessToken = pAccessToken_
    }

-- | A map of custom key-value pairs that you can provide as input for any custom workflows that this action triggers.
--
-- You create custom workflows by assigning AWS Lambda functions to user pool triggers. When you use the UpdateUserAttributes API action, Amazon Cognito invokes the function that is assigned to the /custom message/ trigger. When Amazon Cognito invokes this function, it passes a JSON payload, which the function receives as input. This payload contains a @clientMetadata@ attribute, which provides the data that you assigned to the ClientMetadata parameter in your UpdateUserAttributes request. In your function code in AWS Lambda, you can process the @clientMetadata@ value to enhance your workflow for your specific needs.
-- For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-working-with-aws-lambda-triggers.html Customizing User Pool Workflows with Lambda Triggers> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'clientMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaClientMetadata :: Lens.Lens' UpdateUserAttributes (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uuaClientMetadata = Lens.lens (clientMetadata :: UpdateUserAttributes -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {clientMetadata = a} :: UpdateUserAttributes)
{-# DEPRECATED uuaClientMetadata "Use generic-lens or generic-optics with 'clientMetadata' instead." #-}

-- | An array of name-value pairs representing user attributes.
--
-- For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaUserAttributes :: Lens.Lens' UpdateUserAttributes [AttributeType]
uuaUserAttributes = Lens.lens (userAttributes :: UpdateUserAttributes -> [AttributeType]) (\s a -> s {userAttributes = a} :: UpdateUserAttributes)
{-# DEPRECATED uuaUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The access token for the request to update user attributes.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuaAccessToken :: Lens.Lens' UpdateUserAttributes (Lude.Sensitive Lude.Text)
uuaAccessToken = Lens.lens (accessToken :: UpdateUserAttributes -> Lude.Sensitive Lude.Text) (\s a -> s {accessToken = a} :: UpdateUserAttributes)
{-# DEPRECATED uuaAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

instance Lude.AWSRequest UpdateUserAttributes where
  type Rs UpdateUserAttributes = UpdateUserAttributesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserAttributesResponse'
            Lude.<$> (x Lude..?> "CodeDeliveryDetailsList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateUserAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserAttributes where
  toJSON UpdateUserAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientMetadata" Lude..=) Lude.<$> clientMetadata,
            Lude.Just ("UserAttributes" Lude..= userAttributes),
            Lude.Just ("AccessToken" Lude..= accessToken)
          ]
      )

instance Lude.ToPath UpdateUserAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to update user attributes.
--
-- /See:/ 'mkUpdateUserAttributesResponse' smart constructor.
data UpdateUserAttributesResponse = UpdateUserAttributesResponse'
  { codeDeliveryDetailsList ::
      Lude.Maybe
        [CodeDeliveryDetailsType],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserAttributesResponse' with the minimum fields required to make a request.
--
-- * 'codeDeliveryDetailsList' - The code delivery details list from the server for the request to update user attributes.
-- * 'responseStatus' - The response status code.
mkUpdateUserAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserAttributesResponse
mkUpdateUserAttributesResponse pResponseStatus_ =
  UpdateUserAttributesResponse'
    { codeDeliveryDetailsList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The code delivery details list from the server for the request to update user attributes.
--
-- /Note:/ Consider using 'codeDeliveryDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuarsCodeDeliveryDetailsList :: Lens.Lens' UpdateUserAttributesResponse (Lude.Maybe [CodeDeliveryDetailsType])
uuarsCodeDeliveryDetailsList = Lens.lens (codeDeliveryDetailsList :: UpdateUserAttributesResponse -> Lude.Maybe [CodeDeliveryDetailsType]) (\s a -> s {codeDeliveryDetailsList = a} :: UpdateUserAttributesResponse)
{-# DEPRECATED uuarsCodeDeliveryDetailsList "Use generic-lens or generic-optics with 'codeDeliveryDetailsList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuarsResponseStatus :: Lens.Lens' UpdateUserAttributesResponse Lude.Int
uuarsResponseStatus = Lens.lens (responseStatus :: UpdateUserAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserAttributesResponse)
{-# DEPRECATED uuarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
