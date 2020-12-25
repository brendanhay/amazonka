{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListCompatibleImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns a list of the different Amazon EC2 Amazon Machine Images (AMIs) that are owned by your AWS account that would be supported for use on a Snow device. Currently, supported AMIs are based on the CentOS 7 (x86_64) - with Updates HVM, Ubuntu Server 14.04 LTS (HVM), and Ubuntu 16.04 LTS - Xenial (HVM) images, available on the AWS Marketplace.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListCompatibleImages
  ( -- * Creating a request
    ListCompatibleImages (..),
    mkListCompatibleImages,

    -- ** Request lenses
    lciMaxResults,
    lciNextToken,

    -- * Destructuring the response
    ListCompatibleImagesResponse (..),
    mkListCompatibleImagesResponse,

    -- ** Response lenses
    lcirrsCompatibleImages,
    lcirrsNextToken,
    lcirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkListCompatibleImages' smart constructor.
data ListCompatibleImages = ListCompatibleImages'
  { -- | The maximum number of results for the list of compatible images. Currently, a Snowball Edge device can store 10 AMIs.
    maxResults :: Core.Maybe Core.Natural,
    -- | HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCompatibleImages' value with any optional fields omitted.
mkListCompatibleImages ::
  ListCompatibleImages
mkListCompatibleImages =
  ListCompatibleImages'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results for the list of compatible images. Currently, a Snowball Edge device can store 10 AMIs.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListCompatibleImages (Core.Maybe Core.Natural)
lciMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListCompatibleImages (Core.Maybe Types.String)
lciNextToken = Lens.field @"nextToken"
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListCompatibleImages where
  toJSON ListCompatibleImages {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListCompatibleImages where
  type Rs ListCompatibleImages = ListCompatibleImagesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSIESnowballJobManagementService.ListCompatibleImages"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCompatibleImagesResponse'
            Core.<$> (x Core..:? "CompatibleImages")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCompatibleImages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"compatibleImages" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCompatibleImagesResponse' smart constructor.
data ListCompatibleImagesResponse = ListCompatibleImagesResponse'
  { -- | A JSON-formatted object that describes a compatible AMI, including the ID and name for a Snow device AMI.
    compatibleImages :: Core.Maybe [Types.CompatibleImage],
    -- | Because HTTP requests are stateless, this is the starting point for your next list of returned images.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCompatibleImagesResponse' value with any optional fields omitted.
mkListCompatibleImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCompatibleImagesResponse
mkListCompatibleImagesResponse responseStatus =
  ListCompatibleImagesResponse'
    { compatibleImages = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A JSON-formatted object that describes a compatible AMI, including the ID and name for a Snow device AMI.
--
-- /Note:/ Consider using 'compatibleImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsCompatibleImages :: Lens.Lens' ListCompatibleImagesResponse (Core.Maybe [Types.CompatibleImage])
lcirrsCompatibleImages = Lens.field @"compatibleImages"
{-# DEPRECATED lcirrsCompatibleImages "Use generic-lens or generic-optics with 'compatibleImages' instead." #-}

-- | Because HTTP requests are stateless, this is the starting point for your next list of returned images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsNextToken :: Lens.Lens' ListCompatibleImagesResponse (Core.Maybe Types.String)
lcirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsResponseStatus :: Lens.Lens' ListCompatibleImagesResponse Core.Int
lcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
