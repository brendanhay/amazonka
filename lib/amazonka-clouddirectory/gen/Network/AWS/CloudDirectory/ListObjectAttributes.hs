{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all attributes that are associated with an object.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectAttributes
  ( -- * Creating a request
    ListObjectAttributes (..),
    mkListObjectAttributes,

    -- ** Request lenses
    loaDirectoryArn,
    loaObjectReference,
    loaConsistencyLevel,
    loaFacetFilter,
    loaMaxResults,
    loaNextToken,

    -- * Destructuring the response
    ListObjectAttributesResponse (..),
    mkListObjectAttributesResponse,

    -- ** Response lenses
    loarrsAttributes,
    loarrsNextToken,
    loarrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectAttributes' smart constructor.
data ListObjectAttributes = ListObjectAttributes'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | The reference that identifies the object whose attributes will be listed.
    objectReference :: Types.ObjectReference,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | Used to filter the list of object attributes that are associated with a certain facet.
    facetFilter :: Core.Maybe Types.SchemaFacet,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectAttributes' value with any optional fields omitted.
mkListObjectAttributes ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  ListObjectAttributes
mkListObjectAttributes directoryArn objectReference =
  ListObjectAttributes'
    { directoryArn,
      objectReference,
      consistencyLevel = Core.Nothing,
      facetFilter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaDirectoryArn :: Lens.Lens' ListObjectAttributes Types.Arn
loaDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED loaDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference that identifies the object whose attributes will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaObjectReference :: Lens.Lens' ListObjectAttributes Types.ObjectReference
loaObjectReference = Lens.field @"objectReference"
{-# DEPRECATED loaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaConsistencyLevel :: Lens.Lens' ListObjectAttributes (Core.Maybe Types.ConsistencyLevel)
loaConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED loaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | Used to filter the list of object attributes that are associated with a certain facet.
--
-- /Note:/ Consider using 'facetFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaFacetFilter :: Lens.Lens' ListObjectAttributes (Core.Maybe Types.SchemaFacet)
loaFacetFilter = Lens.field @"facetFilter"
{-# DEPRECATED loaFacetFilter "Use generic-lens or generic-optics with 'facetFilter' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaMaxResults :: Lens.Lens' ListObjectAttributes (Core.Maybe Core.Natural)
loaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED loaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loaNextToken :: Lens.Lens' ListObjectAttributes (Core.Maybe Types.NextToken)
loaNextToken = Lens.field @"nextToken"
{-# DEPRECATED loaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListObjectAttributes where
  toJSON ListObjectAttributes {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("FacetFilter" Core..=) Core.<$> facetFilter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListObjectAttributes where
  type Rs ListObjectAttributes = ListObjectAttributesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/attributes",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectAttributesResponse'
            Core.<$> (x Core..:? "Attributes")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjectAttributes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attributes" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListObjectAttributesResponse' smart constructor.
data ListObjectAttributesResponse = ListObjectAttributesResponse'
  { -- | Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
    attributes :: Core.Maybe [Types.AttributeKeyAndValue],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListObjectAttributesResponse' value with any optional fields omitted.
mkListObjectAttributesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectAttributesResponse
mkListObjectAttributesResponse responseStatus =
  ListObjectAttributesResponse'
    { attributes = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Attributes map that is associated with the object. @AttributeArn@ is the key, and attribute value is the value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarrsAttributes :: Lens.Lens' ListObjectAttributesResponse (Core.Maybe [Types.AttributeKeyAndValue])
loarrsAttributes = Lens.field @"attributes"
{-# DEPRECATED loarrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarrsNextToken :: Lens.Lens' ListObjectAttributesResponse (Core.Maybe Types.NextToken)
loarrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED loarrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loarrsResponseStatus :: Lens.Lens' ListObjectAttributesResponse Core.Int
loarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED loarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
