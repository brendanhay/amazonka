{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.GetUpgradeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the latest status of the last upgrade or upgrade eligibility check that was performed on the domain.
module Network.AWS.ElasticSearch.GetUpgradeStatus
  ( -- * Creating a request
    GetUpgradeStatus (..),
    mkGetUpgradeStatus,

    -- ** Request lenses
    gusDomainName,

    -- * Destructuring the response
    GetUpgradeStatusResponse (..),
    mkGetUpgradeStatusResponse,

    -- ** Response lenses
    gusrrsStepStatus,
    gusrrsUpgradeName,
    gusrrsUpgradeStep,
    gusrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'GetUpgradeStatus' @ operation.
--
-- /See:/ 'mkGetUpgradeStatus' smart constructor.
newtype GetUpgradeStatus = GetUpgradeStatus'
  { domainName :: Types.DomainName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetUpgradeStatus' value with any optional fields omitted.
mkGetUpgradeStatus ::
  -- | 'domainName'
  Types.DomainName ->
  GetUpgradeStatus
mkGetUpgradeStatus domainName = GetUpgradeStatus' {domainName}

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusDomainName :: Lens.Lens' GetUpgradeStatus Types.DomainName
gusDomainName = Lens.field @"domainName"
{-# DEPRECATED gusDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Core.AWSRequest GetUpgradeStatus where
  type Rs GetUpgradeStatus = GetUpgradeStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2015-01-01/es/upgradeDomain/" Core.<> (Core.toText domainName)
                Core.<> ("/status")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUpgradeStatusResponse'
            Core.<$> (x Core..:? "StepStatus")
            Core.<*> (x Core..:? "UpgradeName")
            Core.<*> (x Core..:? "UpgradeStep")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'GetUpgradeStatus' @ operation.
--
-- /See:/ 'mkGetUpgradeStatusResponse' smart constructor.
data GetUpgradeStatusResponse = GetUpgradeStatusResponse'
  { -- | One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:
    --
    --     * In Progress
    --
    --     * Succeeded
    --
    --     * Succeeded with Issues
    --
    --     * Failed
    stepStatus :: Core.Maybe Types.UpgradeStatus,
    -- | A string that describes the update briefly
    upgradeName :: Core.Maybe Types.UpgradeName,
    -- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
    --
    --     * PreUpgradeCheck
    --
    --     * Snapshot
    --
    --     * Upgrade
    upgradeStep :: Core.Maybe Types.UpgradeStep,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUpgradeStatusResponse' value with any optional fields omitted.
mkGetUpgradeStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetUpgradeStatusResponse
mkGetUpgradeStatusResponse responseStatus =
  GetUpgradeStatusResponse'
    { stepStatus = Core.Nothing,
      upgradeName = Core.Nothing,
      upgradeStep = Core.Nothing,
      responseStatus
    }

-- | One of 4 statuses that a step can go through returned as part of the @'GetUpgradeStatusResponse' @ object. The status can take one of the following values:
--
--     * In Progress
--
--     * Succeeded
--
--     * Succeeded with Issues
--
--     * Failed
--
--
--
-- /Note:/ Consider using 'stepStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsStepStatus :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe Types.UpgradeStatus)
gusrrsStepStatus = Lens.field @"stepStatus"
{-# DEPRECATED gusrrsStepStatus "Use generic-lens or generic-optics with 'stepStatus' instead." #-}

-- | A string that describes the update briefly
--
-- /Note:/ Consider using 'upgradeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsUpgradeName :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe Types.UpgradeName)
gusrrsUpgradeName = Lens.field @"upgradeName"
{-# DEPRECATED gusrrsUpgradeName "Use generic-lens or generic-optics with 'upgradeName' instead." #-}

-- | Represents one of 3 steps that an Upgrade or Upgrade Eligibility Check does through:
--
--     * PreUpgradeCheck
--
--     * Snapshot
--
--     * Upgrade
--
--
--
-- /Note:/ Consider using 'upgradeStep' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsUpgradeStep :: Lens.Lens' GetUpgradeStatusResponse (Core.Maybe Types.UpgradeStep)
gusrrsUpgradeStep = Lens.field @"upgradeStep"
{-# DEPRECATED gusrrsUpgradeStep "Use generic-lens or generic-optics with 'upgradeStep' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsResponseStatus :: Lens.Lens' GetUpgradeStatusResponse Core.Int
gusrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gusrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
