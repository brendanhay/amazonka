{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DescribePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a platform version. Provides full details. Compare to 'ListPlatformVersions' , which provides summary information about a list of platform versions.
--
-- For definitions of platform version and other platform-related terms, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary> .
module Network.AWS.ElasticBeanstalk.DescribePlatformVersion
  ( -- * Creating a request
    DescribePlatformVersion (..),
    mkDescribePlatformVersion,

    -- ** Request lenses
    dPlatformArn,

    -- * Destructuring the response
    DescribePlatformVersionResponse (..),
    mkDescribePlatformVersionResponse,

    -- ** Response lenses
    drsPlatformDescription,
    drsResponseStatus,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePlatformVersion' smart constructor.
newtype DescribePlatformVersion = DescribePlatformVersion'
  { -- | The ARN of the platform version.
    platformArn :: Core.Maybe Types.PlatformArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePlatformVersion' value with any optional fields omitted.
mkDescribePlatformVersion ::
  DescribePlatformVersion
mkDescribePlatformVersion =
  DescribePlatformVersion' {platformArn = Core.Nothing}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformArn :: Lens.Lens' DescribePlatformVersion (Core.Maybe Types.PlatformArn)
dPlatformArn = Lens.field @"platformArn"
{-# DEPRECATED dPlatformArn "Use generic-lens or generic-optics with 'platformArn' instead." #-}

instance Core.AWSRequest DescribePlatformVersion where
  type Rs DescribePlatformVersion = DescribePlatformVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribePlatformVersion")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "PlatformArn" Core.<$> platformArn)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribePlatformVersionResult"
      ( \s h x ->
          DescribePlatformVersionResponse'
            Core.<$> (x Core..@? "PlatformDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { -- | Detailed information about the platform version.
    platformDescription :: Core.Maybe Types.PlatformDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribePlatformVersionResponse' value with any optional fields omitted.
mkDescribePlatformVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribePlatformVersionResponse
mkDescribePlatformVersionResponse responseStatus =
  DescribePlatformVersionResponse'
    { platformDescription =
        Core.Nothing,
      responseStatus
    }

-- | Detailed information about the platform version.
--
-- /Note:/ Consider using 'platformDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPlatformDescription :: Lens.Lens' DescribePlatformVersionResponse (Core.Maybe Types.PlatformDescription)
drsPlatformDescription = Lens.field @"platformDescription"
{-# DEPRECATED drsPlatformDescription "Use generic-lens or generic-optics with 'platformDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribePlatformVersionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
