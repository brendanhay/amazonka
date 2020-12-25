{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListObjectPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns policies attached to an object in pagination fashion.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListObjectPolicies
  ( -- * Creating a request
    ListObjectPolicies (..),
    mkListObjectPolicies,

    -- ** Request lenses
    lDirectoryArn,
    lObjectReference,
    lConsistencyLevel,
    lMaxResults,
    lNextToken,

    -- * Destructuring the response
    ListObjectPoliciesResponse (..),
    mkListObjectPoliciesResponse,

    -- ** Response lenses
    loprrsAttachedPolicyIds,
    loprrsNextToken,
    loprrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListObjectPolicies' smart constructor.
data ListObjectPolicies = ListObjectPolicies'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | Reference that identifies the object for which policies will be listed.
    objectReference :: Types.ObjectReference,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectPolicies' value with any optional fields omitted.
mkListObjectPolicies ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'objectReference'
  Types.ObjectReference ->
  ListObjectPolicies
mkListObjectPolicies directoryArn objectReference =
  ListObjectPolicies'
    { directoryArn,
      objectReference,
      consistencyLevel = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDirectoryArn :: Lens.Lens' ListObjectPolicies Types.Arn
lDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED lDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | Reference that identifies the object for which policies will be listed.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lObjectReference :: Lens.Lens' ListObjectPolicies Types.ObjectReference
lObjectReference = Lens.field @"objectReference"
{-# DEPRECATED lObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lConsistencyLevel :: Lens.Lens' ListObjectPolicies (Core.Maybe Types.ConsistencyLevel)
lConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED lConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListObjectPolicies (Core.Maybe Core.Natural)
lMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListObjectPolicies (Core.Maybe Types.NextToken)
lNextToken = Lens.field @"nextToken"
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListObjectPolicies where
  toJSON ListObjectPolicies {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ObjectReference" Core..= objectReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListObjectPolicies where
  type Rs ListObjectPolicies = ListObjectPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/object/policy",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObjectPoliciesResponse'
            Core.<$> (x Core..:? "AttachedPolicyIds")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListObjectPolicies where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attachedPolicyIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListObjectPoliciesResponse' smart constructor.
data ListObjectPoliciesResponse = ListObjectPoliciesResponse'
  { -- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
    attachedPolicyIds :: Core.Maybe [Types.ObjectIdentifier],
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListObjectPoliciesResponse' value with any optional fields omitted.
mkListObjectPoliciesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListObjectPoliciesResponse
mkListObjectPoliciesResponse responseStatus =
  ListObjectPoliciesResponse'
    { attachedPolicyIds = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
--
-- /Note:/ Consider using 'attachedPolicyIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprrsAttachedPolicyIds :: Lens.Lens' ListObjectPoliciesResponse (Core.Maybe [Types.ObjectIdentifier])
loprrsAttachedPolicyIds = Lens.field @"attachedPolicyIds"
{-# DEPRECATED loprrsAttachedPolicyIds "Use generic-lens or generic-optics with 'attachedPolicyIds' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprrsNextToken :: Lens.Lens' ListObjectPoliciesResponse (Core.Maybe Types.NextToken)
loprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED loprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loprrsResponseStatus :: Lens.Lens' ListObjectPoliciesResponse Core.Int
loprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED loprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
