{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.GetSchemaCreationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of a schema creation operation.
module Network.AWS.AppSync.GetSchemaCreationStatus
    (
    -- * Creating a request
      GetSchemaCreationStatus (..)
    , mkGetSchemaCreationStatus
    -- ** Request lenses
    , gscsApiId

    -- * Destructuring the response
    , GetSchemaCreationStatusResponse (..)
    , mkGetSchemaCreationStatusResponse
    -- ** Response lenses
    , gscsrrsDetails
    , gscsrrsStatus
    , gscsrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSchemaCreationStatus' smart constructor.
newtype GetSchemaCreationStatus = GetSchemaCreationStatus'
  { apiId :: Core.Text
    -- ^ The API ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaCreationStatus' value with any optional fields omitted.
mkGetSchemaCreationStatus
    :: Core.Text -- ^ 'apiId'
    -> GetSchemaCreationStatus
mkGetSchemaCreationStatus apiId = GetSchemaCreationStatus'{apiId}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsApiId :: Lens.Lens' GetSchemaCreationStatus Core.Text
gscsApiId = Lens.field @"apiId"
{-# INLINEABLE gscsApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

instance Core.ToQuery GetSchemaCreationStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSchemaCreationStatus where
        toHeaders GetSchemaCreationStatus{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSchemaCreationStatus where
        type Rs GetSchemaCreationStatus = GetSchemaCreationStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/schemacreation",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSchemaCreationStatusResponse' Core.<$>
                   (x Core..:? "details") Core.<*> x Core..:? "status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSchemaCreationStatusResponse' smart constructor.
data GetSchemaCreationStatusResponse = GetSchemaCreationStatusResponse'
  { details :: Core.Maybe Core.Text
    -- ^ Detailed information about the status of the schema creation operation.
  , status :: Core.Maybe Types.SchemaStatus
    -- ^ The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSchemaCreationStatusResponse' value with any optional fields omitted.
mkGetSchemaCreationStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSchemaCreationStatusResponse
mkGetSchemaCreationStatusResponse responseStatus
  = GetSchemaCreationStatusResponse'{details = Core.Nothing,
                                     status = Core.Nothing, responseStatus}

-- | Detailed information about the status of the schema creation operation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsDetails :: Lens.Lens' GetSchemaCreationStatusResponse (Core.Maybe Core.Text)
gscsrrsDetails = Lens.field @"details"
{-# INLINEABLE gscsrrsDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsStatus :: Lens.Lens' GetSchemaCreationStatusResponse (Core.Maybe Types.SchemaStatus)
gscsrrsStatus = Lens.field @"status"
{-# INLINEABLE gscsrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscsrrsResponseStatus :: Lens.Lens' GetSchemaCreationStatusResponse Core.Int
gscsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gscsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
