{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.StartSchemaCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new schema to your GraphQL API.
--
-- This operation is asynchronous. Use to determine when it has completed.
module Network.AWS.AppSync.StartSchemaCreation
    (
    -- * Creating a request
      StartSchemaCreation (..)
    , mkStartSchemaCreation
    -- ** Request lenses
    , sscApiId
    , sscDefinition

    -- * Destructuring the response
    , StartSchemaCreationResponse (..)
    , mkStartSchemaCreationResponse
    -- ** Response lenses
    , sscrrsStatus
    , sscrrsResponseStatus
    ) where

import qualified Network.AWS.AppSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartSchemaCreation' smart constructor.
data StartSchemaCreation = StartSchemaCreation'
  { apiId :: Core.Text
    -- ^ The API ID.
  , definition :: Core.Base64
    -- ^ The schema definition, in GraphQL schema language format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaCreation' value with any optional fields omitted.
mkStartSchemaCreation
    :: Core.Text -- ^ 'apiId'
    -> Core.Base64 -- ^ 'definition'
    -> StartSchemaCreation
mkStartSchemaCreation apiId definition
  = StartSchemaCreation'{apiId, definition}

-- | The API ID.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscApiId :: Lens.Lens' StartSchemaCreation Core.Text
sscApiId = Lens.field @"apiId"
{-# INLINEABLE sscApiId #-}
{-# DEPRECATED apiId "Use generic-lens or generic-optics with 'apiId' instead"  #-}

-- | The schema definition, in GraphQL schema language format.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscDefinition :: Lens.Lens' StartSchemaCreation Core.Base64
sscDefinition = Lens.field @"definition"
{-# INLINEABLE sscDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

instance Core.ToQuery StartSchemaCreation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartSchemaCreation where
        toHeaders StartSchemaCreation{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartSchemaCreation where
        toJSON StartSchemaCreation{..}
          = Core.object
              (Core.catMaybes [Core.Just ("definition" Core..= definition)])

instance Core.AWSRequest StartSchemaCreation where
        type Rs StartSchemaCreation = StartSchemaCreationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/v1/apis/" Core.<> Core.toText apiId Core.<> "/schemacreation",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartSchemaCreationResponse' Core.<$>
                   (x Core..:? "status") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartSchemaCreationResponse' smart constructor.
data StartSchemaCreationResponse = StartSchemaCreationResponse'
  { status :: Core.Maybe Types.SchemaStatus
    -- ^ The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartSchemaCreationResponse' value with any optional fields omitted.
mkStartSchemaCreationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartSchemaCreationResponse
mkStartSchemaCreationResponse responseStatus
  = StartSchemaCreationResponse'{status = Core.Nothing,
                                 responseStatus}

-- | The current state of the schema (PROCESSING, FAILED, SUCCESS, or NOT_APPLICABLE). When the schema is in the ACTIVE state, you can add data.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrrsStatus :: Lens.Lens' StartSchemaCreationResponse (Core.Maybe Types.SchemaStatus)
sscrrsStatus = Lens.field @"status"
{-# INLINEABLE sscrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sscrrsResponseStatus :: Lens.Lens' StartSchemaCreationResponse Core.Int
sscrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sscrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
