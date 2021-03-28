{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.PutBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.PutBlockPublicAccessConfiguration
    (
    -- * Creating a request
      PutBlockPublicAccessConfiguration (..)
    , mkPutBlockPublicAccessConfiguration
    -- ** Request lenses
    , pbpacBlockPublicAccessConfiguration

    -- * Destructuring the response
    , PutBlockPublicAccessConfigurationResponse (..)
    , mkPutBlockPublicAccessConfigurationResponse
    -- ** Response lenses
    , pbpacrrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutBlockPublicAccessConfiguration' smart constructor.
newtype PutBlockPublicAccessConfiguration = PutBlockPublicAccessConfiguration'
  { blockPublicAccessConfiguration :: Types.BlockPublicAccessConfiguration
    -- ^ A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutBlockPublicAccessConfiguration' value with any optional fields omitted.
mkPutBlockPublicAccessConfiguration
    :: Types.BlockPublicAccessConfiguration -- ^ 'blockPublicAccessConfiguration'
    -> PutBlockPublicAccessConfiguration
mkPutBlockPublicAccessConfiguration blockPublicAccessConfiguration
  = PutBlockPublicAccessConfiguration'{blockPublicAccessConfiguration}

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
--
-- /Note:/ Consider using 'blockPublicAccessConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpacBlockPublicAccessConfiguration :: Lens.Lens' PutBlockPublicAccessConfiguration Types.BlockPublicAccessConfiguration
pbpacBlockPublicAccessConfiguration = Lens.field @"blockPublicAccessConfiguration"
{-# INLINEABLE pbpacBlockPublicAccessConfiguration #-}
{-# DEPRECATED blockPublicAccessConfiguration "Use generic-lens or generic-optics with 'blockPublicAccessConfiguration' instead"  #-}

instance Core.ToQuery PutBlockPublicAccessConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutBlockPublicAccessConfiguration where
        toHeaders PutBlockPublicAccessConfiguration{..}
          = Core.pure
              ("X-Amz-Target",
               "ElasticMapReduce.PutBlockPublicAccessConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutBlockPublicAccessConfiguration where
        toJSON PutBlockPublicAccessConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("BlockPublicAccessConfiguration" Core..=
                       blockPublicAccessConfiguration)])

instance Core.AWSRequest PutBlockPublicAccessConfiguration where
        type Rs PutBlockPublicAccessConfiguration =
             PutBlockPublicAccessConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutBlockPublicAccessConfigurationResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBlockPublicAccessConfigurationResponse' smart constructor.
newtype PutBlockPublicAccessConfigurationResponse = PutBlockPublicAccessConfigurationResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutBlockPublicAccessConfigurationResponse' value with any optional fields omitted.
mkPutBlockPublicAccessConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutBlockPublicAccessConfigurationResponse
mkPutBlockPublicAccessConfigurationResponse responseStatus
  = PutBlockPublicAccessConfigurationResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbpacrrsResponseStatus :: Lens.Lens' PutBlockPublicAccessConfigurationResponse Core.Int
pbpacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pbpacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
