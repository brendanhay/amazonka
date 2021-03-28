{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateElasticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered Elastic IP address's name. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateElasticIp
    (
    -- * Creating a request
      UpdateElasticIp (..)
    , mkUpdateElasticIp
    -- ** Request lenses
    , ueiElasticIp
    , ueiName

    -- * Destructuring the response
    , UpdateElasticIpResponse (..)
    , mkUpdateElasticIpResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateElasticIp' smart constructor.
data UpdateElasticIp = UpdateElasticIp'
  { elasticIp :: Core.Text
    -- ^ The IP address for which you want to update the name.
  , name :: Core.Maybe Core.Text
    -- ^ The new name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateElasticIp' value with any optional fields omitted.
mkUpdateElasticIp
    :: Core.Text -- ^ 'elasticIp'
    -> UpdateElasticIp
mkUpdateElasticIp elasticIp
  = UpdateElasticIp'{elasticIp, name = Core.Nothing}

-- | The IP address for which you want to update the name.
--
-- /Note:/ Consider using 'elasticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueiElasticIp :: Lens.Lens' UpdateElasticIp Core.Text
ueiElasticIp = Lens.field @"elasticIp"
{-# INLINEABLE ueiElasticIp #-}
{-# DEPRECATED elasticIp "Use generic-lens or generic-optics with 'elasticIp' instead"  #-}

-- | The new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueiName :: Lens.Lens' UpdateElasticIp (Core.Maybe Core.Text)
ueiName = Lens.field @"name"
{-# INLINEABLE ueiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery UpdateElasticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateElasticIp where
        toHeaders UpdateElasticIp{..}
          = Core.pure ("X-Amz-Target", "OpsWorks_20130218.UpdateElasticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateElasticIp where
        toJSON UpdateElasticIp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ElasticIp" Core..= elasticIp),
                  ("Name" Core..=) Core.<$> name])

instance Core.AWSRequest UpdateElasticIp where
        type Rs UpdateElasticIp = UpdateElasticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateElasticIpResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateElasticIpResponse' smart constructor.
data UpdateElasticIpResponse = UpdateElasticIpResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateElasticIpResponse' value with any optional fields omitted.
mkUpdateElasticIpResponse
    :: UpdateElasticIpResponse
mkUpdateElasticIpResponse = UpdateElasticIpResponse'
