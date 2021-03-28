{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an on-premises instance. 
module Network.AWS.CodeDeploy.GetOnPremisesInstance
    (
    -- * Creating a request
      GetOnPremisesInstance (..)
    , mkGetOnPremisesInstance
    -- ** Request lenses
    , gopiInstanceName

    -- * Destructuring the response
    , GetOnPremisesInstanceResponse (..)
    , mkGetOnPremisesInstanceResponse
    -- ** Response lenses
    , gopirrsInstanceInfo
    , gopirrsResponseStatus
    ) where

import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetOnPremisesInstance@ operation. 
--
-- /See:/ 'mkGetOnPremisesInstance' smart constructor.
newtype GetOnPremisesInstance = GetOnPremisesInstance'
  { instanceName :: Types.InstanceName
    -- ^ The name of the on-premises instance about which to get information. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOnPremisesInstance' value with any optional fields omitted.
mkGetOnPremisesInstance
    :: Types.InstanceName -- ^ 'instanceName'
    -> GetOnPremisesInstance
mkGetOnPremisesInstance instanceName
  = GetOnPremisesInstance'{instanceName}

-- | The name of the on-premises instance about which to get information. 
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopiInstanceName :: Lens.Lens' GetOnPremisesInstance Types.InstanceName
gopiInstanceName = Lens.field @"instanceName"
{-# INLINEABLE gopiInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery GetOnPremisesInstance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOnPremisesInstance where
        toHeaders GetOnPremisesInstance{..}
          = Core.pure
              ("X-Amz-Target", "CodeDeploy_20141006.GetOnPremisesInstance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOnPremisesInstance where
        toJSON GetOnPremisesInstance{..}
          = Core.object
              (Core.catMaybes [Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest GetOnPremisesInstance where
        type Rs GetOnPremisesInstance = GetOnPremisesInstanceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOnPremisesInstanceResponse' Core.<$>
                   (x Core..:? "instanceInfo") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the output of a @GetOnPremisesInstance@ operation. 
--
-- /See:/ 'mkGetOnPremisesInstanceResponse' smart constructor.
data GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'
  { instanceInfo :: Core.Maybe Types.InstanceInfo
    -- ^ Information about the on-premises instance. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOnPremisesInstanceResponse' value with any optional fields omitted.
mkGetOnPremisesInstanceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOnPremisesInstanceResponse
mkGetOnPremisesInstanceResponse responseStatus
  = GetOnPremisesInstanceResponse'{instanceInfo = Core.Nothing,
                                   responseStatus}

-- | Information about the on-premises instance. 
--
-- /Note:/ Consider using 'instanceInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopirrsInstanceInfo :: Lens.Lens' GetOnPremisesInstanceResponse (Core.Maybe Types.InstanceInfo)
gopirrsInstanceInfo = Lens.field @"instanceInfo"
{-# INLINEABLE gopirrsInstanceInfo #-}
{-# DEPRECATED instanceInfo "Use generic-lens or generic-optics with 'instanceInfo' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gopirrsResponseStatus :: Lens.Lens' GetOnPremisesInstanceResponse Core.Int
gopirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gopirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
