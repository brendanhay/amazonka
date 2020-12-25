{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeRulesPackages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the rules packages that are specified by the ARNs of the rules packages.
module Network.AWS.Inspector.DescribeRulesPackages
  ( -- * Creating a request
    DescribeRulesPackages (..),
    mkDescribeRulesPackages,

    -- ** Request lenses
    drpRulesPackageArns,
    drpLocale,

    -- * Destructuring the response
    DescribeRulesPackagesResponse (..),
    mkDescribeRulesPackagesResponse,

    -- ** Response lenses
    drprrsRulesPackages,
    drprrsFailedItems,
    drprrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRulesPackages' smart constructor.
data DescribeRulesPackages = DescribeRulesPackages'
  { -- | The ARN that specifies the rules package that you want to describe.
    rulesPackageArns :: Core.NonEmpty Types.Arn,
    -- | The locale that you want to translate a rules package description into.
    locale :: Core.Maybe Types.Locale
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRulesPackages' value with any optional fields omitted.
mkDescribeRulesPackages ::
  -- | 'rulesPackageArns'
  Core.NonEmpty Types.Arn ->
  DescribeRulesPackages
mkDescribeRulesPackages rulesPackageArns =
  DescribeRulesPackages' {rulesPackageArns, locale = Core.Nothing}

-- | The ARN that specifies the rules package that you want to describe.
--
-- /Note:/ Consider using 'rulesPackageArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpRulesPackageArns :: Lens.Lens' DescribeRulesPackages (Core.NonEmpty Types.Arn)
drpRulesPackageArns = Lens.field @"rulesPackageArns"
{-# DEPRECATED drpRulesPackageArns "Use generic-lens or generic-optics with 'rulesPackageArns' instead." #-}

-- | The locale that you want to translate a rules package description into.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLocale :: Lens.Lens' DescribeRulesPackages (Core.Maybe Types.Locale)
drpLocale = Lens.field @"locale"
{-# DEPRECATED drpLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

instance Core.FromJSON DescribeRulesPackages where
  toJSON DescribeRulesPackages {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("rulesPackageArns" Core..= rulesPackageArns),
            ("locale" Core..=) Core.<$> locale
          ]
      )

instance Core.AWSRequest DescribeRulesPackages where
  type Rs DescribeRulesPackages = DescribeRulesPackagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "InspectorService.DescribeRulesPackages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRulesPackagesResponse'
            Core.<$> (x Core..:? "rulesPackages" Core..!= Core.mempty)
            Core.<*> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeRulesPackagesResponse' smart constructor.
data DescribeRulesPackagesResponse = DescribeRulesPackagesResponse'
  { -- | Information about the rules package.
    rulesPackages :: [Types.RulesPackage],
    -- | Rules package details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRulesPackagesResponse' value with any optional fields omitted.
mkDescribeRulesPackagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRulesPackagesResponse
mkDescribeRulesPackagesResponse responseStatus =
  DescribeRulesPackagesResponse'
    { rulesPackages = Core.mempty,
      failedItems = Core.mempty,
      responseStatus
    }

-- | Information about the rules package.
--
-- /Note:/ Consider using 'rulesPackages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsRulesPackages :: Lens.Lens' DescribeRulesPackagesResponse [Types.RulesPackage]
drprrsRulesPackages = Lens.field @"rulesPackages"
{-# DEPRECATED drprrsRulesPackages "Use generic-lens or generic-optics with 'rulesPackages' instead." #-}

-- | Rules package details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsFailedItems :: Lens.Lens' DescribeRulesPackagesResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
drprrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED drprrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DescribeRulesPackagesResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
