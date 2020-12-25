{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need to delete your domain to onboard again using SSO. Use with caution. All of the members of the domain will lose access to their EFS volume, including data, notebooks, and other artifacts.
module Network.AWS.SageMaker.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    ddgDomainId,
    ddgRetentionPolicy,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The domain ID.
    domainId :: Types.DomainId,
    -- | The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
    retentionPolicy :: Core.Maybe Types.RetentionPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomain' value with any optional fields omitted.
mkDeleteDomain ::
  -- | 'domainId'
  Types.DomainId ->
  DeleteDomain
mkDeleteDomain domainId =
  DeleteDomain' {domainId, retentionPolicy = Core.Nothing}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgDomainId :: Lens.Lens' DeleteDomain Types.DomainId
ddgDomainId = Lens.field @"domainId"
{-# DEPRECATED ddgDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
--
-- /Note:/ Consider using 'retentionPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddgRetentionPolicy :: Lens.Lens' DeleteDomain (Core.Maybe Types.RetentionPolicy)
ddgRetentionPolicy = Lens.field @"retentionPolicy"
{-# DEPRECATED ddgRetentionPolicy "Use generic-lens or generic-optics with 'retentionPolicy' instead." #-}

instance Core.FromJSON DeleteDomain where
  toJSON DeleteDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            ("RetentionPolicy" Core..=) Core.<$> retentionPolicy
          ]
      )

instance Core.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DeleteDomain")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull DeleteDomainResponse'

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDomainResponse' value with any optional fields omitted.
mkDeleteDomainResponse ::
  DeleteDomainResponse
mkDeleteDomainResponse = DeleteDomainResponse'
