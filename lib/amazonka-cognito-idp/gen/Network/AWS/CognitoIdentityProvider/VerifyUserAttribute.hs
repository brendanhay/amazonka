{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies the specified user attributes in the user pool.
module Network.AWS.CognitoIdentityProvider.VerifyUserAttribute
  ( -- * Creating a request
    VerifyUserAttribute (..),
    mkVerifyUserAttribute,

    -- ** Request lenses
    vuaAccessToken,
    vuaAttributeName,
    vuaCode,

    -- * Destructuring the response
    VerifyUserAttributeResponse (..),
    mkVerifyUserAttributeResponse,

    -- ** Response lenses
    vuarrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to verify user attributes.
--
-- /See:/ 'mkVerifyUserAttribute' smart constructor.
data VerifyUserAttribute = VerifyUserAttribute'
  { -- | Represents the access token of the request to verify user attributes.
    accessToken :: Types.TokenModelType,
    -- | The attribute name in the request to verify user attributes.
    attributeName :: Types.AttributeNameType,
    -- | The verification code in the request to verify user attributes.
    code :: Types.ConfirmationCodeType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyUserAttribute' value with any optional fields omitted.
mkVerifyUserAttribute ::
  -- | 'accessToken'
  Types.TokenModelType ->
  -- | 'attributeName'
  Types.AttributeNameType ->
  -- | 'code'
  Types.ConfirmationCodeType ->
  VerifyUserAttribute
mkVerifyUserAttribute accessToken attributeName code =
  VerifyUserAttribute' {accessToken, attributeName, code}

-- | Represents the access token of the request to verify user attributes.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaAccessToken :: Lens.Lens' VerifyUserAttribute Types.TokenModelType
vuaAccessToken = Lens.field @"accessToken"
{-# DEPRECATED vuaAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The attribute name in the request to verify user attributes.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaAttributeName :: Lens.Lens' VerifyUserAttribute Types.AttributeNameType
vuaAttributeName = Lens.field @"attributeName"
{-# DEPRECATED vuaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The verification code in the request to verify user attributes.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuaCode :: Lens.Lens' VerifyUserAttribute Types.ConfirmationCodeType
vuaCode = Lens.field @"code"
{-# DEPRECATED vuaCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Core.FromJSON VerifyUserAttribute where
  toJSON VerifyUserAttribute {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("AttributeName" Core..= attributeName),
            Core.Just ("Code" Core..= code)
          ]
      )

instance Core.AWSRequest VerifyUserAttribute where
  type Rs VerifyUserAttribute = VerifyUserAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.VerifyUserAttribute"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          VerifyUserAttributeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | A container representing the response from the server from the request to verify user attributes.
--
-- /See:/ 'mkVerifyUserAttributeResponse' smart constructor.
newtype VerifyUserAttributeResponse = VerifyUserAttributeResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyUserAttributeResponse' value with any optional fields omitted.
mkVerifyUserAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  VerifyUserAttributeResponse
mkVerifyUserAttributeResponse responseStatus =
  VerifyUserAttributeResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vuarrsResponseStatus :: Lens.Lens' VerifyUserAttributeResponse Core.Int
vuarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vuarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
