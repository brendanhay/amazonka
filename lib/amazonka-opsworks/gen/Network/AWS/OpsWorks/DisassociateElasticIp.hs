{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DisassociateElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Elastic IP address from its instance. The address remains registered with the stack. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DisassociateElasticIp
    (
    -- * Creating a request
      DisassociateElasticIp (..)
    , mkDisassociateElasticIp
    -- ** Request lenses
    , deiElasticIp

    -- * Destructuring the response
    , DisassociateElasticIpResponse (..)
    , mkDisassociateElasticIpResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateElasticIp' smart constructor.
newtype DisassociateElasticIp = DisassociateElasticIp'
  { elasticIp :: Core.Text
    -- ^ The Elastic IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateElasticIp' value with any optional fields omitted.
mkDisassociateElasticIp
    :: Core.Text -- ^ 'elasticIp'
    -> DisassociateElasticIp
mkDisassociateElasticIp elasticIp
  = DisassociateElasticIp'{elasticIp}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deiElasticIp :: Lens.Lens' DisassociateElasticIp Core.Text
deiElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE deiElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

instance Core.ToQuery DisassociateElasticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateElasticIp where
        toHeaders DisassociateElasticIp{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorks_20130218.DisassociateElasticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateElasticIp where
        toJSON DisassociateElasticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ElasticIp" Core..= elasticIp)])

instance Core.AWSRequest DisassociateElasticIp where
        type Rs DisassociateElasticIp = DisassociateElasticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DisassociateElasticIpResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateElasticIpResponse' smart constructor.
data DisassociateElasticIpResponse = DisassociateElasticIpResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateElasticIpResponse' value with any optional fields omitted.
mkDisassociateElasticIpResponse
    :: DisassociateElasticIpResponse
mkDisassociateElasticIpResponse = DisassociateElasticIpResponse'
