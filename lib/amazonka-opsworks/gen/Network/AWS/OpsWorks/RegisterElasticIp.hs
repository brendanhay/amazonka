{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an Elastic IP address with a specified stack. An address can be registered with only one stack at a time. If the address is already registered, you must first deregister it by calling 'DeregisterElasticIp' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.RegisterElasticIp
    (
    -- * Creating a request
      RegisterElasticIp (..)
    , mkRegisterElasticIp
    -- ** Request lenses
    , reiElasticIp
    , reiStackId

    -- * Destructuring the response
    , RegisterElasticIpResponse (..)
    , mkRegisterElasticIpResponse
    -- ** Response lenses
    , reirrsElasticIp
    , reirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterElasticIp' smart constructor.
data RegisterElasticIp = RegisterElasticIp'
  { elasticIp :: Core.Text
    -- ^ The Elastic IP address.
  , stackId :: Core.Text
    -- ^ The stack ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterElasticIp' value with any optional fields omitted.
mkRegisterElasticIp
    :: Core.Text -- ^ 'elasticIp'
    -> Core.Text -- ^ 'stackId'
    -> RegisterElasticIp
mkRegisterElasticIp elasticIp stackId
  = RegisterElasticIp'{elasticIp, stackId}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiElasticIp :: Lens.Lens' RegisterElasticIp Core.Text
reiElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE reiElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reiStackId :: Lens.Lens' RegisterElasticIp Core.Text
reiStackId = Lens.field @"stackId"
{-# INLINEABLE reiStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

instance Core.ToQuery RegisterElasticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterElasticIp where
        toHeaders RegisterElasticIp{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.RegisterElasticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterElasticIp where
        toJSON RegisterElasticIp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ElasticIp" Core..= elasticIp),
                  Core.Just ("StackId" Core..= stackId)])

instance Core.AWSRequest RegisterElasticIp where
        type Rs RegisterElasticIp = RegisterElasticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterElasticIpResponse' Core.<$>
                   (x Core..:? "ElasticIp") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the response to a @RegisterElasticIp@ request.
--
-- /See:/ 'mkRegisterElasticIpResponse' smart constructor.
data RegisterElasticIpResponse = RegisterElasticIpResponse'
  { elasticIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP address.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterElasticIpResponse' value with any optional fields omitted.
mkRegisterElasticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterElasticIpResponse
mkRegisterElasticIpResponse responseStatus
  = RegisterElasticIpResponse'{elasticIp = Core.Nothing,
                               responseStatus}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirrsElasticIp :: Lens.Lens' RegisterElasticIpResponse (Core.Maybe Core.Text)
reirrsElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE reirrsElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reirrsResponseStatus :: Lens.Lens' RegisterElasticIpResponse Core.Int
reirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE reirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
