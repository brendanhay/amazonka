{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssociateElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one of the stack's registered Elastic IP addresses with a specified instance. The address must first be registered with the stack by calling 'RegisterElasticIp' . For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.AssociateElasticIp
    (
    -- * Creating a request
      AssociateElasticIp (..)
    , mkAssociateElasticIp
    -- ** Request lenses
    , aeiElasticIp
    , aeiInstanceId

    -- * Destructuring the response
    , AssociateElasticIpResponse (..)
    , mkAssociateElasticIpResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateElasticIp' smart constructor.
data AssociateElasticIp = AssociateElasticIp'
  { elasticIp :: Core.Text
    -- ^ The Elastic IP address.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateElasticIp' value with any optional fields omitted.
mkAssociateElasticIp
    :: Core.Text -- ^ 'elasticIp'
    -> AssociateElasticIp
mkAssociateElasticIp elasticIp
  = AssociateElasticIp'{elasticIp, instanceId = Core.Nothing}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiElasticIp :: Lens.Lens' AssociateElasticIp Core.Text
aeiElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE aeiElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiInstanceId :: Lens.Lens' AssociateElasticIp (Core.Maybe Core.Text)
aeiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aeiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

instance Core.ToQuery AssociateElasticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateElasticIp where
        toHeaders AssociateElasticIp{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.AssociateElasticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateElasticIp where
        toJSON AssociateElasticIp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ElasticIp" Core..= elasticIp),
                  ("InstanceId" Core..=) Core.<$> instanceId])

instance Core.AWSRequest AssociateElasticIp where
        type Rs AssociateElasticIp = AssociateElasticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull AssociateElasticIpResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateElasticIpResponse' smart constructor.
data AssociateElasticIpResponse = AssociateElasticIpResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateElasticIpResponse' value with any optional fields omitted.
mkAssociateElasticIpResponse
    :: AssociateElasticIpResponse
mkAssociateElasticIpResponse = AssociateElasticIpResponse'
