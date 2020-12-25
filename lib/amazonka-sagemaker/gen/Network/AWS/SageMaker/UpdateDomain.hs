{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the default settings for new user profiles in the domain.
module Network.AWS.SageMaker.UpdateDomain
  ( -- * Creating a request
    UpdateDomain (..),
    mkUpdateDomain,

    -- ** Request lenses
    udDomainId,
    udDefaultUserSettings,

    -- * Destructuring the response
    UpdateDomainResponse (..),
    mkUpdateDomainResponse,

    -- ** Response lenses
    udrrsDomainArn,
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateDomain' smart constructor.
data UpdateDomain = UpdateDomain'
  { -- | The ID of the domain to be updated.
    domainId :: Types.DomainId,
    -- | A collection of settings.
    defaultUserSettings :: Core.Maybe Types.UserSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomain' value with any optional fields omitted.
mkUpdateDomain ::
  -- | 'domainId'
  Types.DomainId ->
  UpdateDomain
mkUpdateDomain domainId =
  UpdateDomain' {domainId, defaultUserSettings = Core.Nothing}

-- | The ID of the domain to be updated.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDomainId :: Lens.Lens' UpdateDomain Types.DomainId
udDomainId = Lens.field @"domainId"
{-# DEPRECATED udDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'defaultUserSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDefaultUserSettings :: Lens.Lens' UpdateDomain (Core.Maybe Types.UserSettings)
udDefaultUserSettings = Lens.field @"defaultUserSettings"
{-# DEPRECATED udDefaultUserSettings "Use generic-lens or generic-optics with 'defaultUserSettings' instead." #-}

instance Core.FromJSON UpdateDomain where
  toJSON UpdateDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            ("DefaultUserSettings" Core..=) Core.<$> defaultUserSettings
          ]
      )

instance Core.AWSRequest UpdateDomain where
  type Rs UpdateDomain = UpdateDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.UpdateDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDomainResponse'
            Core.<$> (x Core..:? "DomainArn") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDomainResponse' smart constructor.
data UpdateDomainResponse = UpdateDomainResponse'
  { -- | The Amazon Resource Name (ARN) of the domain.
    domainArn :: Core.Maybe Types.DomainArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDomainResponse' value with any optional fields omitted.
mkUpdateDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDomainResponse
mkUpdateDomainResponse responseStatus =
  UpdateDomainResponse' {domainArn = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the domain.
--
-- /Note:/ Consider using 'domainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsDomainArn :: Lens.Lens' UpdateDomainResponse (Core.Maybe Types.DomainArn)
udrrsDomainArn = Lens.field @"domainArn"
{-# DEPRECATED udrrsDomainArn "Use generic-lens or generic-optics with 'domainArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDomainResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
