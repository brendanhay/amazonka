{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to either upgrade your domain or perform an Upgrade eligibility check to a compatible Elasticsearch version.
module Network.AWS.ElasticSearch.UpgradeElasticsearchDomain
  ( -- * Creating a request
    UpgradeElasticsearchDomain (..),
    mkUpgradeElasticsearchDomain,

    -- ** Request lenses
    uedDomainName,
    uedTargetVersion,
    uedPerformCheckOnly,

    -- * Destructuring the response
    UpgradeElasticsearchDomainResponse (..),
    mkUpgradeElasticsearchDomainResponse,

    -- ** Response lenses
    uedrrsDomainName,
    uedrrsPerformCheckOnly,
    uedrrsTargetVersion,
    uedrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticSearch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for request parameters to @'UpgradeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkUpgradeElasticsearchDomain' smart constructor.
data UpgradeElasticsearchDomain = UpgradeElasticsearchDomain'
  { domainName :: Types.DomainName,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Types.ElasticsearchVersionString,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeElasticsearchDomain' value with any optional fields omitted.
mkUpgradeElasticsearchDomain ::
  -- | 'domainName'
  Types.DomainName ->
  -- | 'targetVersion'
  Types.ElasticsearchVersionString ->
  UpgradeElasticsearchDomain
mkUpgradeElasticsearchDomain domainName targetVersion =
  UpgradeElasticsearchDomain'
    { domainName,
      targetVersion,
      performCheckOnly = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedDomainName :: Lens.Lens' UpgradeElasticsearchDomain Types.DomainName
uedDomainName = Lens.field @"domainName"
{-# DEPRECATED uedDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The version of Elasticsearch that you intend to upgrade the domain to.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedTargetVersion :: Lens.Lens' UpgradeElasticsearchDomain Types.ElasticsearchVersionString
uedTargetVersion = Lens.field @"targetVersion"
{-# DEPRECATED uedTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- /Note:/ Consider using 'performCheckOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedPerformCheckOnly :: Lens.Lens' UpgradeElasticsearchDomain (Core.Maybe Core.Bool)
uedPerformCheckOnly = Lens.field @"performCheckOnly"
{-# DEPRECATED uedPerformCheckOnly "Use generic-lens or generic-optics with 'performCheckOnly' instead." #-}

instance Core.FromJSON UpgradeElasticsearchDomain where
  toJSON UpgradeElasticsearchDomain {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainName" Core..= domainName),
            Core.Just ("TargetVersion" Core..= targetVersion),
            ("PerformCheckOnly" Core..=) Core.<$> performCheckOnly
          ]
      )

instance Core.AWSRequest UpgradeElasticsearchDomain where
  type
    Rs UpgradeElasticsearchDomain =
      UpgradeElasticsearchDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2015-01-01/es/upgradeDomain",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpgradeElasticsearchDomainResponse'
            Core.<$> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "PerformCheckOnly")
            Core.<*> (x Core..:? "TargetVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Container for response returned by @'UpgradeElasticsearchDomain' @ operation.
--
-- /See:/ 'mkUpgradeElasticsearchDomainResponse' smart constructor.
data UpgradeElasticsearchDomainResponse = UpgradeElasticsearchDomainResponse'
  { domainName :: Core.Maybe Types.DomainName,
    -- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
    performCheckOnly :: Core.Maybe Core.Bool,
    -- | The version of Elasticsearch that you intend to upgrade the domain to.
    targetVersion :: Core.Maybe Types.ElasticsearchVersionString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpgradeElasticsearchDomainResponse' value with any optional fields omitted.
mkUpgradeElasticsearchDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpgradeElasticsearchDomainResponse
mkUpgradeElasticsearchDomainResponse responseStatus =
  UpgradeElasticsearchDomainResponse'
    { domainName = Core.Nothing,
      performCheckOnly = Core.Nothing,
      targetVersion = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrrsDomainName :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Types.DomainName)
uedrrsDomainName = Lens.field @"domainName"
{-# DEPRECATED uedrrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | This flag, when set to True, indicates that an Upgrade Eligibility Check needs to be performed. This will not actually perform the Upgrade.
--
-- /Note:/ Consider using 'performCheckOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrrsPerformCheckOnly :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Core.Bool)
uedrrsPerformCheckOnly = Lens.field @"performCheckOnly"
{-# DEPRECATED uedrrsPerformCheckOnly "Use generic-lens or generic-optics with 'performCheckOnly' instead." #-}

-- | The version of Elasticsearch that you intend to upgrade the domain to.
--
-- /Note:/ Consider using 'targetVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrrsTargetVersion :: Lens.Lens' UpgradeElasticsearchDomainResponse (Core.Maybe Types.ElasticsearchVersionString)
uedrrsTargetVersion = Lens.field @"targetVersion"
{-# DEPRECATED uedrrsTargetVersion "Use generic-lens or generic-optics with 'targetVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uedrrsResponseStatus :: Lens.Lens' UpgradeElasticsearchDomainResponse Core.Int
uedrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uedrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
