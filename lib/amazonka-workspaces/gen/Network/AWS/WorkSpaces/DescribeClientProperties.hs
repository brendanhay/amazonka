{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeClientProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified Amazon WorkSpaces clients.
module Network.AWS.WorkSpaces.DescribeClientProperties
    (
    -- * Creating a request
      DescribeClientProperties (..)
    , mkDescribeClientProperties
    -- ** Request lenses
    , dcpResourceIds

    -- * Destructuring the response
    , DescribeClientPropertiesResponse (..)
    , mkDescribeClientPropertiesResponse
    -- ** Response lenses
    , dcprrsClientPropertiesList
    , dcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeClientProperties' smart constructor.
newtype DescribeClientProperties = DescribeClientProperties'
  { resourceIds :: Core.NonEmpty Types.NonEmptyString
    -- ^ The resource identifier, in the form of directory IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientProperties' value with any optional fields omitted.
mkDescribeClientProperties
    :: Core.NonEmpty Types.NonEmptyString -- ^ 'resourceIds'
    -> DescribeClientProperties
mkDescribeClientProperties resourceIds
  = DescribeClientProperties'{resourceIds}

-- | The resource identifier, in the form of directory IDs.
--
-- /Note:/ Consider using 'resourceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpResourceIds :: Lens.Lens' DescribeClientProperties (Core.NonEmpty Types.NonEmptyString)
dcpResourceIds = Lens.field @"resourceIds"
{-# INLINEABLE dcpResourceIds #-}
{-# DEPRECATED resourceIds "Use generic-lens or generic-optics with 'resourceIds' instead"  #-}

instance Core.ToQuery DescribeClientProperties where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeClientProperties where
        toHeaders DescribeClientProperties{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DescribeClientProperties")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeClientProperties where
        toJSON DescribeClientProperties{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceIds" Core..= resourceIds)])

instance Core.AWSRequest DescribeClientProperties where
        type Rs DescribeClientProperties = DescribeClientPropertiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeClientPropertiesResponse' Core.<$>
                   (x Core..:? "ClientPropertiesList") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeClientPropertiesResponse' smart constructor.
data DescribeClientPropertiesResponse = DescribeClientPropertiesResponse'
  { clientPropertiesList :: Core.Maybe [Types.ClientPropertiesResult]
    -- ^ Information about the specified Amazon WorkSpaces clients.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeClientPropertiesResponse' value with any optional fields omitted.
mkDescribeClientPropertiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeClientPropertiesResponse
mkDescribeClientPropertiesResponse responseStatus
  = DescribeClientPropertiesResponse'{clientPropertiesList =
                                        Core.Nothing,
                                      responseStatus}

-- | Information about the specified Amazon WorkSpaces clients.
--
-- /Note:/ Consider using 'clientPropertiesList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsClientPropertiesList :: Lens.Lens' DescribeClientPropertiesResponse (Core.Maybe [Types.ClientPropertiesResult])
dcprrsClientPropertiesList = Lens.field @"clientPropertiesList"
{-# INLINEABLE dcprrsClientPropertiesList #-}
{-# DEPRECATED clientPropertiesList "Use generic-lens or generic-optics with 'clientPropertiesList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeClientPropertiesResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
