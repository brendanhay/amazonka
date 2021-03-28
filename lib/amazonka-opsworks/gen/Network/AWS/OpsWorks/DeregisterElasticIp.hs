{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeregisterElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Elastic IP address. The address can then be registered by another stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeregisterElasticIp
    (
    -- * Creating a request
      DeregisterElasticIp (..)
    , mkDeregisterElasticIp
    -- ** Request lenses
    , dElasticIp

    -- * Destructuring the response
    , DeregisterElasticIpResponse (..)
    , mkDeregisterElasticIpResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeregisterElasticIp' smart constructor.
newtype DeregisterElasticIp = DeregisterElasticIp'
  { elasticIp :: Core.Text
    -- ^ The Elastic IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterElasticIp' value with any optional fields omitted.
mkDeregisterElasticIp
    :: Core.Text -- ^ 'elasticIp'
    -> DeregisterElasticIp
mkDeregisterElasticIp elasticIp = DeregisterElasticIp'{elasticIp}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dElasticIp :: Lens.Lens' DeregisterElasticIp Core.Text
dElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE dElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

instance Core.ToQuery DeregisterElasticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterElasticIp where
        toHeaders DeregisterElasticIp{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DeregisterElasticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterElasticIp where
        toJSON DeregisterElasticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ElasticIp" Core..= elasticIp)])

instance Core.AWSRequest DeregisterElasticIp where
        type Rs DeregisterElasticIp = DeregisterElasticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeregisterElasticIpResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterElasticIpResponse' smart constructor.
data DeregisterElasticIpResponse = DeregisterElasticIpResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterElasticIpResponse' value with any optional fields omitted.
mkDeregisterElasticIpResponse
    :: DeregisterElasticIpResponse
mkDeregisterElasticIpResponse = DeregisterElasticIpResponse'
