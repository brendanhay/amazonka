{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AddCustomAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional user attributes to the user pool schema.
module Network.AWS.CognitoIdentityProvider.AddCustomAttributes
  ( -- * Creating a request
    AddCustomAttributes (..),
    mkAddCustomAttributes,

    -- ** Request lenses
    acaUserPoolId,
    acaCustomAttributes,

    -- * Destructuring the response
    AddCustomAttributesResponse (..),
    mkAddCustomAttributesResponse,

    -- ** Response lenses
    acarsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to add custom attributes.
--
-- /See:/ 'mkAddCustomAttributes' smart constructor.
data AddCustomAttributes = AddCustomAttributes'
  { -- | The user pool ID for the user pool where you want to add custom attributes.
    userPoolId :: Lude.Text,
    -- | An array of custom attributes, such as Mutable and Name.
    customAttributes :: Lude.NonEmpty SchemaAttributeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCustomAttributes' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID for the user pool where you want to add custom attributes.
-- * 'customAttributes' - An array of custom attributes, such as Mutable and Name.
mkAddCustomAttributes ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'customAttributes'
  Lude.NonEmpty SchemaAttributeType ->
  AddCustomAttributes
mkAddCustomAttributes pUserPoolId_ pCustomAttributes_ =
  AddCustomAttributes'
    { userPoolId = pUserPoolId_,
      customAttributes = pCustomAttributes_
    }

-- | The user pool ID for the user pool where you want to add custom attributes.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaUserPoolId :: Lens.Lens' AddCustomAttributes Lude.Text
acaUserPoolId = Lens.lens (userPoolId :: AddCustomAttributes -> Lude.Text) (\s a -> s {userPoolId = a} :: AddCustomAttributes)
{-# DEPRECATED acaUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | An array of custom attributes, such as Mutable and Name.
--
-- /Note:/ Consider using 'customAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acaCustomAttributes :: Lens.Lens' AddCustomAttributes (Lude.NonEmpty SchemaAttributeType)
acaCustomAttributes = Lens.lens (customAttributes :: AddCustomAttributes -> Lude.NonEmpty SchemaAttributeType) (\s a -> s {customAttributes = a} :: AddCustomAttributes)
{-# DEPRECATED acaCustomAttributes "Use generic-lens or generic-optics with 'customAttributes' instead." #-}

instance Lude.AWSRequest AddCustomAttributes where
  type Rs AddCustomAttributes = AddCustomAttributesResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddCustomAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddCustomAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.AddCustomAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddCustomAttributes where
  toJSON AddCustomAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("CustomAttributes" Lude..= customAttributes)
          ]
      )

instance Lude.ToPath AddCustomAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery AddCustomAttributes where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to add custom attributes.
--
-- /See:/ 'mkAddCustomAttributesResponse' smart constructor.
newtype AddCustomAttributesResponse = AddCustomAttributesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCustomAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddCustomAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddCustomAttributesResponse
mkAddCustomAttributesResponse pResponseStatus_ =
  AddCustomAttributesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acarsResponseStatus :: Lens.Lens' AddCustomAttributesResponse Lude.Int
acarsResponseStatus = Lens.lens (responseStatus :: AddCustomAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddCustomAttributesResponse)
{-# DEPRECATED acarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
