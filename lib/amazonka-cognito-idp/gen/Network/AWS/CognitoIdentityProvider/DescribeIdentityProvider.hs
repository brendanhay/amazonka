{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific identity provider.
module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
  ( -- * Creating a request
    DescribeIdentityProvider (..),
    mkDescribeIdentityProvider,

    -- ** Request lenses
    dipUserPoolId,
    dipProviderName,

    -- * Destructuring the response
    DescribeIdentityProviderResponse (..),
    mkDescribeIdentityProviderResponse,

    -- ** Response lenses
    diprrsIdentityProvider,
    diprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Types.UserPoolIdType,
    -- | The identity provider name.
    providerName :: Types.ProviderNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeIdentityProvider' value with any optional fields omitted.
mkDescribeIdentityProvider ::
  -- | 'userPoolId'
  Types.UserPoolIdType ->
  -- | 'providerName'
  Types.ProviderNameType ->
  DescribeIdentityProvider
mkDescribeIdentityProvider userPoolId providerName =
  DescribeIdentityProvider' {userPoolId, providerName}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipUserPoolId :: Lens.Lens' DescribeIdentityProvider Types.UserPoolIdType
dipUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED dipUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identity provider name.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipProviderName :: Lens.Lens' DescribeIdentityProvider Types.ProviderNameType
dipProviderName = Lens.field @"providerName"
{-# DEPRECATED dipProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Core.FromJSON DescribeIdentityProvider where
  toJSON DescribeIdentityProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName)
          ]
      )

instance Core.AWSRequest DescribeIdentityProvider where
  type Rs DescribeIdentityProvider = DescribeIdentityProviderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.DescribeIdentityProvider"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderResponse'
            Core.<$> (x Core..: "IdentityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { -- | The identity provider that was deleted.
    identityProvider :: Types.IdentityProviderType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeIdentityProviderResponse' value with any optional fields omitted.
mkDescribeIdentityProviderResponse ::
  -- | 'identityProvider'
  Types.IdentityProviderType ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeIdentityProviderResponse
mkDescribeIdentityProviderResponse identityProvider responseStatus =
  DescribeIdentityProviderResponse'
    { identityProvider,
      responseStatus
    }

-- | The identity provider that was deleted.
--
-- /Note:/ Consider using 'identityProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsIdentityProvider :: Lens.Lens' DescribeIdentityProviderResponse Types.IdentityProviderType
diprrsIdentityProvider = Lens.field @"identityProvider"
{-# DEPRECATED diprrsIdentityProvider "Use generic-lens or generic-optics with 'identityProvider' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DescribeIdentityProviderResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
