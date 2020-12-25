{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetObjectInformation (..),
    mkGetObjectInformation,

    -- ** Request lenses
    goiDirectoryArn,
    goiObjectReference,
    goiConsistencyLevel,

    -- * Destructuring the response
    GetObjectInformationResponse (..),
    mkGetObjectInformationResponse,

    -- ** Response lenses
    goirrsObjectIdentifier,
    goirrsSchemaFacets,
    goirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { -- | The ARN of the directory being retrieved.
    directoryArn :: Types.Arn,
    -- | A reference to the object.
    objectReference :: Types.ObjectReference,
    -- | The consistency level at which to retrieve the object information.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectInformation' value with any optional fields omitted.
mkGetObjectInformation ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  GetObjectInformation
mkGetObjectInformation directoryArn objectReference =
  GetObjectInformation'
    { directoryArn,
      objectReference,
      consistencyLevel = Core.Nothing
    }

-- | The ARN of the directory being retrieved.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiDirectoryArn :: Lens.Lens' GetObjectInformation Types.Arn
goiDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED goiDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | A reference to the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiObjectReference :: Lens.Lens' GetObjectInformation Types.ObjectReference
goiObjectReference = Lens.field @"objectReference"
{-# DEPRECATED goiObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | The consistency level at which to retrieve the object information.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiConsistencyLevel :: Lens.Lens' GetObjectInformation (Core.Maybe Types.ConsistencyLevel)
goiConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED goiConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

instance Core.FromJSON GetObjectInformation where
  toJSON GetObjectInformation {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ObjectReference" Core..= objectReference)]
      )

instance Core.AWSRequest GetObjectInformation where
  type Rs GetObjectInformation = GetObjectInformationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/information",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetObjectInformationResponse'
            Core.<$> (x Core..:? "ObjectIdentifier")
            Core.<*> (x Core..:? "SchemaFacets")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { -- | The @ObjectIdentifier@ of the specified object.
    objectIdentifier :: Core.Maybe Types.ObjectIdentifier,
    -- | The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
    schemaFacets :: Core.Maybe [Types.SchemaFacet],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectInformationResponse' value with any optional fields omitted.
mkGetObjectInformationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetObjectInformationResponse
mkGetObjectInformationResponse responseStatus =
  GetObjectInformationResponse'
    { objectIdentifier = Core.Nothing,
      schemaFacets = Core.Nothing,
      responseStatus
    }

-- | The @ObjectIdentifier@ of the specified object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsObjectIdentifier :: Lens.Lens' GetObjectInformationResponse (Core.Maybe Types.ObjectIdentifier)
goirrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# DEPRECATED goirrsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsSchemaFacets :: Lens.Lens' GetObjectInformationResponse (Core.Maybe [Types.SchemaFacet])
goirrsSchemaFacets = Lens.field @"schemaFacets"
{-# DEPRECATED goirrsSchemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirrsResponseStatus :: Lens.Lens' GetObjectInformationResponse Core.Int
goirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED goirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
