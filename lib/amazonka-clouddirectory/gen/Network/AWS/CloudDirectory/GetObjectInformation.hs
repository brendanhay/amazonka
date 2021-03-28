{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetObjectInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about an object.
module Network.AWS.CloudDirectory.GetObjectInformation
    (
    -- * Creating a request
      GetObjectInformation (..)
    , mkGetObjectInformation
    -- ** Request lenses
    , goiDirectoryArn
    , goiObjectReference
    , goiConsistencyLevel

    -- * Destructuring the response
    , GetObjectInformationResponse (..)
    , mkGetObjectInformationResponse
    -- ** Response lenses
    , goirrsObjectIdentifier
    , goirrsSchemaFacets
    , goirrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory being retrieved.
  , objectReference :: Types.ObjectReference
    -- ^ A reference to the object.
  , consistencyLevel :: Core.Maybe Types.ConsistencyLevel
    -- ^ The consistency level at which to retrieve the object information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectInformation' value with any optional fields omitted.
mkGetObjectInformation
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.ObjectReference -- ^ 'objectReference'
    -> GetObjectInformation
mkGetObjectInformation directoryArn objectReference
  = GetObjectInformation'{directoryArn, objectReference,
                          consistencyLevel = Core.Nothing}

-- | The ARN of the directory being retrieved.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiDirectoryArn :: Lens.Lens' GetObjectInformation Types.Arn
goiDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE goiDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | A reference to the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiObjectReference :: Lens.Lens' GetObjectInformation Types.ObjectReference
goiObjectReference = Lens.field @"objectReference"
{-# INLINEABLE goiObjectReference #-}
{-# DEPRECATED objectReference "Use generic-lens or generic-optics with 'objectReference' instead"  #-}

-- | The consistency level at which to retrieve the object information.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiConsistencyLevel :: Lens.Lens' GetObjectInformation (Core.Maybe Types.ConsistencyLevel)
goiConsistencyLevel = Lens.field @"consistencyLevel"
{-# INLINEABLE goiConsistencyLevel #-}
{-# DEPRECATED consistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead"  #-}

instance Core.ToQuery GetObjectInformation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetObjectInformation where
        toHeaders GetObjectInformation{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn Core.<>
              Core.toHeaders "x-amz-consistency-level" consistencyLevel

instance Core.FromJSON GetObjectInformation where
        toJSON GetObjectInformation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ObjectReference" Core..= objectReference)])

instance Core.AWSRequest GetObjectInformation where
        type Rs GetObjectInformation = GetObjectInformationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/amazonclouddirectory/2017-01-11/object/information",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetObjectInformationResponse' Core.<$>
                   (x Core..:? "ObjectIdentifier") Core.<*> x Core..:? "SchemaFacets"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the specified object.
  , schemaFacets :: Core.Maybe [Types.SchemaFacet]
    -- ^ The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectInformationResponse' value with any optional fields omitted.
mkGetObjectInformationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetObjectInformationResponse
mkGetObjectInformationResponse responseStatus
  = GetObjectInformationResponse'{objectIdentifier = Core.Nothing,
                                  schemaFacets = Core.Nothing, responseStatus}

-- | The @ObjectIdentifier@ of the specified object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsObjectIdentifier :: Lens.Lens' GetObjectInformationResponse (Core.Maybe Types.ObjectIdentifier)
goirrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE goirrsObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsSchemaFacets :: Lens.Lens' GetObjectInformationResponse (Core.Maybe [Types.SchemaFacet])
goirrsSchemaFacets = Lens.field @"schemaFacets"
{-# INLINEABLE goirrsSchemaFacets #-}
{-# DEPRECATED schemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsResponseStatus :: Lens.Lens' GetObjectInformationResponse Core.Int
goirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
