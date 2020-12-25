{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
  ( -- * Creating a request
    DeleteIdentityProvider (..),
    mkDeleteIdentityProvider,

    -- ** Request lenses
    dipfUserPoolId,
    dipfProviderName,

    -- * Destructuring the response
    DeleteIdentityProviderResponse (..),
    mkDeleteIdentityProviderResponse,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolId,
    -- | The identity provider name.
    providerName :: Types.ProviderNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityProvider' value with any optional fields omitted.
mkDeleteIdentityProvider ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'providerName'
  Types.ProviderNameType ->
  DeleteIdentityProvider
mkDeleteIdentityProvider userPoolId providerName =
  DeleteIdentityProvider' {userPoolId, providerName}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfUserPoolId :: Lens.Lens' DeleteIdentityProvider Types.UserPoolId
dipfUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dipfUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipfProviderName :: Lens.Lens' DeleteIdentityProvider Types.ProviderNameType
dipfProviderName = Lens.field @"providerName"
{-# DEPRECATED dipfProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Core.FromJSON DeleteIdentityProvider where
  toJSON DeleteIdentityProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName)
          ]
      )

instance Core.AWSRequest DeleteIdentityProvider where
  type Rs DeleteIdentityProvider = DeleteIdentityProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DeleteIdentityProvider"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteIdentityProviderResponse'

-- | /See:/ 'mkDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteIdentityProviderResponse' value with any optional fields omitted.
mkDeleteIdentityProviderResponse ::
  DeleteIdentityProviderResponse
mkDeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
