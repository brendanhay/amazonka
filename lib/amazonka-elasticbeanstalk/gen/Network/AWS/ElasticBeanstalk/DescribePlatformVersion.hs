{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribePlatformVersion (..)
    , mkDescribePlatformVersion
    -- ** Request lenses
    , dPlatformArn

    -- * Destructuring the response
    , DescribePlatformVersionResponse (..)
    , mkDescribePlatformVersionResponse
    -- ** Response lenses
    , drsPlatformDescription
    , drsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePlatformVersion' smart constructor.
newtype DescribePlatformVersion = DescribePlatformVersion'
  { platformArn :: Core.Maybe Types.PlatformArn
    -- ^ The ARN of the platform version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePlatformVersion' value with any optional fields omitted.
mkDescribePlatformVersion
    :: DescribePlatformVersion
mkDescribePlatformVersion
  = DescribePlatformVersion'{platformArn = Core.Nothing}

-- | The ARN of the platform version.
--
-- /Note:/ Consider using 'platformArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPlatformArn :: Lens.Lens' DescribePlatformVersion (Core.Maybe Types.PlatformArn)
dPlatformArn = Lens.field @"platformArn"
{-# INLINEABLE dPlatformArn #-}
{-# DEPRECATED platformArn "Use generic-lens or generic-optics with 'platformArn' instead"  #-}

instance Core.ToQuery DescribePlatformVersion where
        toQuery DescribePlatformVersion{..}
          = Core.toQueryPair "Action"
              ("DescribePlatformVersion" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PlatformArn") platformArn

instance Core.ToHeaders DescribePlatformVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePlatformVersion where
        type Rs DescribePlatformVersion = DescribePlatformVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribePlatformVersionResult"
              (\ s h x ->
                 DescribePlatformVersionResponse' Core.<$>
                   (x Core..@? "PlatformDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { platformDescription :: Core.Maybe Types.PlatformDescription
    -- ^ Detailed information about the platform version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribePlatformVersionResponse' value with any optional fields omitted.
mkDescribePlatformVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePlatformVersionResponse
mkDescribePlatformVersionResponse responseStatus
  = DescribePlatformVersionResponse'{platformDescription =
                                       Core.Nothing,
                                     responseStatus}

-- | Detailed information about the platform version.
--
-- /Note:/ Consider using 'platformDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsPlatformDescription :: Lens.Lens' DescribePlatformVersionResponse (Core.Maybe Types.PlatformDescription)
drsPlatformDescription = Lens.field @"platformDescription"
{-# INLINEABLE drsPlatformDescription #-}
{-# DEPRECATED platformDescription "Use generic-lens or generic-optics with 'platformDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribePlatformVersionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
