{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
  ( -- * Creating a request
    ListPolicyAttachments (..),
    mkListPolicyAttachments,

    -- ** Request lenses
    lpaDirectoryArn,
    lpaPolicyReference,
    lpaConsistencyLevel,
    lpaMaxResults,
    lpaNextToken,

    -- * Destructuring the response
    ListPolicyAttachmentsResponse (..),
    mkListPolicyAttachmentsResponse,

    -- ** Response lenses
    lparrsNextToken,
    lparrsObjectIdentifiers,
    lparrsResponseStatus,
  )
where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
    directoryArn :: Types.Arn,
    -- | The reference that identifies the policy object.
    policyReference :: Types.ObjectReference,
    -- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
    consistencyLevel :: Core.Maybe Types.ConsistencyLevel,
    -- | The maximum number of items to be retrieved in a single call. This is an approximate number.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyAttachments' value with any optional fields omitted.
mkListPolicyAttachments ::
  -- | 'directoryArn'
  Types.Arn ->
  -- | 'policyReference'
  Types.ObjectReference ->
  ListPolicyAttachments
mkListPolicyAttachments directoryArn policyReference =
  ListPolicyAttachments'
    { directoryArn,
      policyReference,
      consistencyLevel = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaDirectoryArn :: Lens.Lens' ListPolicyAttachments Types.Arn
lpaDirectoryArn = Lens.field @"directoryArn"
{-# DEPRECATED lpaDirectoryArn "Use generic-lens or generic-optics with 'directoryArn' instead." #-}

-- | The reference that identifies the policy object.
--
-- /Note:/ Consider using 'policyReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaPolicyReference :: Lens.Lens' ListPolicyAttachments Types.ObjectReference
lpaPolicyReference = Lens.field @"policyReference"
{-# DEPRECATED lpaPolicyReference "Use generic-lens or generic-optics with 'policyReference' instead." #-}

-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaConsistencyLevel :: Lens.Lens' ListPolicyAttachments (Core.Maybe Types.ConsistencyLevel)
lpaConsistencyLevel = Lens.field @"consistencyLevel"
{-# DEPRECATED lpaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaMaxResults :: Lens.Lens' ListPolicyAttachments (Core.Maybe Core.Natural)
lpaMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpaNextToken :: Lens.Lens' ListPolicyAttachments (Core.Maybe Types.NextToken)
lpaNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListPolicyAttachments where
  toJSON ListPolicyAttachments {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyReference" Core..= policyReference),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListPolicyAttachments where
  type Rs ListPolicyAttachments = ListPolicyAttachmentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/amazonclouddirectory/2017-01-11/policy/attachment",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "x-amz-data-partition" directoryArn
            Core.<> (Core.toHeaders "x-amz-consistency-level" consistencyLevel),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyAttachmentsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "ObjectIdentifiers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPolicyAttachments where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"objectIdentifiers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { -- | The pagination token.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @ObjectIdentifiers@ to which the policy is attached.
    objectIdentifiers :: Core.Maybe [Types.ObjectIdentifier],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyAttachmentsResponse' value with any optional fields omitted.
mkListPolicyAttachmentsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPolicyAttachmentsResponse
mkListPolicyAttachmentsResponse responseStatus =
  ListPolicyAttachmentsResponse'
    { nextToken = Core.Nothing,
      objectIdentifiers = Core.Nothing,
      responseStatus
    }

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsNextToken :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe Types.NextToken)
lparrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lparrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- /Note:/ Consider using 'objectIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsObjectIdentifiers :: Lens.Lens' ListPolicyAttachmentsResponse (Core.Maybe [Types.ObjectIdentifier])
lparrsObjectIdentifiers = Lens.field @"objectIdentifiers"
{-# DEPRECATED lparrsObjectIdentifiers "Use generic-lens or generic-optics with 'objectIdentifiers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lparrsResponseStatus :: Lens.Lens' ListPolicyAttachmentsResponse Core.Int
lparrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lparrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
