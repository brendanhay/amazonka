{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an OpsItem by using the ID. You must have permission in AWS Identity and Access Management (IAM) to view information about an OpsItem. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ . 
module Network.AWS.SSM.GetOpsItem
    (
    -- * Creating a request
      GetOpsItem (..)
    , mkGetOpsItem
    -- ** Request lenses
    , goiOpsItemId

    -- * Destructuring the response
    , GetOpsItemResponse (..)
    , mkGetOpsItemResponse
    -- ** Response lenses
    , goirrsOpsItem
    , goirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetOpsItem' smart constructor.
newtype GetOpsItem = GetOpsItem'
  { opsItemId :: Types.OpsItemId
    -- ^ The ID of the OpsItem that you want to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpsItem' value with any optional fields omitted.
mkGetOpsItem
    :: Types.OpsItemId -- ^ 'opsItemId'
    -> GetOpsItem
mkGetOpsItem opsItemId = GetOpsItem'{opsItemId}

-- | The ID of the OpsItem that you want to get.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiOpsItemId :: Lens.Lens' GetOpsItem Types.OpsItemId
goiOpsItemId = Lens.field @"opsItemId"
{-# INLINEABLE goiOpsItemId #-}
{-# DEPRECATED opsItemId "Use generic-lens or generic-optics with 'opsItemId' instead"  #-}

instance Core.ToQuery GetOpsItem where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOpsItem where
        toHeaders GetOpsItem{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetOpsItem") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOpsItem where
        toJSON GetOpsItem{..}
          = Core.object
              (Core.catMaybes [Core.Just ("OpsItemId" Core..= opsItemId)])

instance Core.AWSRequest GetOpsItem where
        type Rs GetOpsItem = GetOpsItemResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOpsItemResponse' Core.<$>
                   (x Core..:? "OpsItem") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetOpsItemResponse' smart constructor.
data GetOpsItemResponse = GetOpsItemResponse'
  { opsItem :: Core.Maybe Types.OpsItem
    -- ^ The OpsItem.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetOpsItemResponse' value with any optional fields omitted.
mkGetOpsItemResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOpsItemResponse
mkGetOpsItemResponse responseStatus
  = GetOpsItemResponse'{opsItem = Core.Nothing, responseStatus}

-- | The OpsItem.
--
-- /Note:/ Consider using 'opsItem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsOpsItem :: Lens.Lens' GetOpsItemResponse (Core.Maybe Types.OpsItem)
goirrsOpsItem = Lens.field @"opsItem"
{-# INLINEABLE goirrsOpsItem #-}
{-# DEPRECATED opsItem "Use generic-lens or generic-optics with 'opsItem' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsResponseStatus :: Lens.Lens' GetOpsItemResponse Core.Int
goirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
