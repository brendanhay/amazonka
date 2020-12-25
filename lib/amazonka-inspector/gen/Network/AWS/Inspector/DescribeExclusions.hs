{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.DescribeExclusions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the exclusions that are specified by the exclusions' ARNs.
module Network.AWS.Inspector.DescribeExclusions
  ( -- * Creating a request
    DescribeExclusions (..),
    mkDescribeExclusions,

    -- ** Request lenses
    deExclusionArns,
    deLocale,

    -- * Destructuring the response
    DescribeExclusionsResponse (..),
    mkDescribeExclusionsResponse,

    -- ** Response lenses
    derrsExclusions,
    derrsFailedItems,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.Inspector.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExclusions' smart constructor.
data DescribeExclusions = DescribeExclusions'
  { -- | The list of ARNs that specify the exclusions that you want to describe.
    exclusionArns :: Core.NonEmpty Types.Arn,
    -- | The locale into which you want to translate the exclusion's title, description, and recommendation.
    locale :: Core.Maybe Types.Locale
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExclusions' value with any optional fields omitted.
mkDescribeExclusions ::
  -- | 'exclusionArns'
  Core.NonEmpty Types.Arn ->
  DescribeExclusions
mkDescribeExclusions exclusionArns =
  DescribeExclusions' {exclusionArns, locale = Core.Nothing}

-- | The list of ARNs that specify the exclusions that you want to describe.
--
-- /Note:/ Consider using 'exclusionArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deExclusionArns :: Lens.Lens' DescribeExclusions (Core.NonEmpty Types.Arn)
deExclusionArns = Lens.field @"exclusionArns"
{-# DEPRECATED deExclusionArns "Use generic-lens or generic-optics with 'exclusionArns' instead." #-}

-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- /Note:/ Consider using 'locale' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deLocale :: Lens.Lens' DescribeExclusions (Core.Maybe Types.Locale)
deLocale = Lens.field @"locale"
{-# DEPRECATED deLocale "Use generic-lens or generic-optics with 'locale' instead." #-}

instance Core.FromJSON DescribeExclusions where
  toJSON DescribeExclusions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("exclusionArns" Core..= exclusionArns),
            ("locale" Core..=) Core.<$> locale
          ]
      )

instance Core.AWSRequest DescribeExclusions where
  type Rs DescribeExclusions = DescribeExclusionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "InspectorService.DescribeExclusions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExclusionsResponse'
            Core.<$> (x Core..:? "exclusions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "failedItems" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeExclusionsResponse' smart constructor.
data DescribeExclusionsResponse = DescribeExclusionsResponse'
  { -- | Information about the exclusions.
    exclusions :: Core.HashMap Types.Arn Types.Exclusion,
    -- | Exclusion details that cannot be described. An error code is provided for each failed item.
    failedItems :: Core.HashMap Types.Arn Types.FailedItemDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExclusionsResponse' value with any optional fields omitted.
mkDescribeExclusionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeExclusionsResponse
mkDescribeExclusionsResponse responseStatus =
  DescribeExclusionsResponse'
    { exclusions = Core.mempty,
      failedItems = Core.mempty,
      responseStatus
    }

-- | Information about the exclusions.
--
-- /Note:/ Consider using 'exclusions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsExclusions :: Lens.Lens' DescribeExclusionsResponse (Core.HashMap Types.Arn Types.Exclusion)
derrsExclusions = Lens.field @"exclusions"
{-# DEPRECATED derrsExclusions "Use generic-lens or generic-optics with 'exclusions' instead." #-}

-- | Exclusion details that cannot be described. An error code is provided for each failed item.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsFailedItems :: Lens.Lens' DescribeExclusionsResponse (Core.HashMap Types.Arn Types.FailedItemDetails)
derrsFailedItems = Lens.field @"failedItems"
{-# DEPRECATED derrsFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeExclusionsResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
