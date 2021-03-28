{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the configuration JSON.
module Network.AWS.EMR.DescribeSecurityConfiguration
    (
    -- * Creating a request
      DescribeSecurityConfiguration (..)
    , mkDescribeSecurityConfiguration
    -- ** Request lenses
    , dName

    -- * Destructuring the response
    , DescribeSecurityConfigurationResponse (..)
    , mkDescribeSecurityConfigurationResponse
    -- ** Response lenses
    , dscrfrsCreationDateTime
    , dscrfrsName
    , dscrfrsSecurityConfiguration
    , dscrfrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeSecurityConfiguration' smart constructor.
newtype DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { name :: Types.XmlString
    -- ^ The name of the security configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSecurityConfiguration' value with any optional fields omitted.
mkDescribeSecurityConfiguration
    :: Types.XmlString -- ^ 'name'
    -> DescribeSecurityConfiguration
mkDescribeSecurityConfiguration name
  = DescribeSecurityConfiguration'{name}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DescribeSecurityConfiguration Types.XmlString
dName = Lens.field @"name"
{-# INLINEABLE dName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DescribeSecurityConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSecurityConfiguration where
        toHeaders DescribeSecurityConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.DescribeSecurityConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSecurityConfiguration where
        toJSON DescribeSecurityConfiguration{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DescribeSecurityConfiguration where
        type Rs DescribeSecurityConfiguration =
             DescribeSecurityConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSecurityConfigurationResponse' Core.<$>
                   (x Core..:? "CreationDateTime") Core.<*> x Core..:? "Name" Core.<*>
                     x Core..:? "SecurityConfiguration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the security configuration was created
  , name :: Core.Maybe Types.XmlString
    -- ^ The name of the security configuration.
  , securityConfiguration :: Core.Maybe Core.Text
    -- ^ The security configuration details in JSON format.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeSecurityConfigurationResponse' value with any optional fields omitted.
mkDescribeSecurityConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSecurityConfigurationResponse
mkDescribeSecurityConfigurationResponse responseStatus
  = DescribeSecurityConfigurationResponse'{creationDateTime =
                                             Core.Nothing,
                                           name = Core.Nothing,
                                           securityConfiguration = Core.Nothing, responseStatus}

-- | The date and time the security configuration was created
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsCreationDateTime :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.NominalDiffTime)
dscrfrsCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE dscrfrsCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The name of the security configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsName :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Types.XmlString)
dscrfrsName = Lens.field @"name"
{-# INLINEABLE dscrfrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The security configuration details in JSON format.
--
-- /Note:/ Consider using 'securityConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsSecurityConfiguration :: Lens.Lens' DescribeSecurityConfigurationResponse (Core.Maybe Core.Text)
dscrfrsSecurityConfiguration = Lens.field @"securityConfiguration"
{-# INLINEABLE dscrfrsSecurityConfiguration #-}
{-# DEPRECATED securityConfiguration "Use generic-lens or generic-optics with 'securityConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscrfrsResponseStatus :: Lens.Lens' DescribeSecurityConfigurationResponse Core.Int
dscrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dscrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
