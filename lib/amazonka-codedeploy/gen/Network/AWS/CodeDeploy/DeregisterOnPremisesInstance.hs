{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters an on-premises instance.
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
    (
    -- * Creating a request
      DeregisterOnPremisesInstance (..)
    , mkDeregisterOnPremisesInstance
    -- ** Request lenses
    , dopiInstanceName

    -- * Destructuring the response
    , DeregisterOnPremisesInstanceResponse (..)
    , mkDeregisterOnPremisesInstanceResponse
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeregisterOnPremisesInstance@ operation.
--
-- /See:/ 'mkDeregisterOnPremisesInstance' smart constructor.
newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance'
  { instanceName :: Types.InstanceName
    -- ^ The name of the on-premises instance to deregister.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterOnPremisesInstance' value with any optional fields omitted.
mkDeregisterOnPremisesInstance
    :: Types.InstanceName -- ^ 'instanceName'
    -> DeregisterOnPremisesInstance
mkDeregisterOnPremisesInstance instanceName
  = DeregisterOnPremisesInstance'{instanceName}

-- | The name of the on-premises instance to deregister.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dopiInstanceName :: Lens.Lens' DeregisterOnPremisesInstance Types.InstanceName
dopiInstanceName = Lens.field @"instanceName"
{-# INLINEABLE dopiInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery DeregisterOnPremisesInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeregisterOnPremisesInstance where
        toHeaders DeregisterOnPremisesInstance{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeDeploy_20141006.DeregisterOnPremisesInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeregisterOnPremisesInstance where
        toJSON DeregisterOnPremisesInstance{..}
          = Core.object
              (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest DeregisterOnPremisesInstance where
        type Rs DeregisterOnPremisesInstance =
             DeregisterOnPremisesInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull DeregisterOnPremisesInstanceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeregisterOnPremisesInstanceResponse' smart constructor.
data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeregisterOnPremisesInstanceResponse' value with any optional fields omitted.
mkDeregisterOnPremisesInstanceResponse
    :: DeregisterOnPremisesInstanceResponse
mkDeregisterOnPremisesInstanceResponse
  = DeregisterOnPremisesInstanceResponse'
