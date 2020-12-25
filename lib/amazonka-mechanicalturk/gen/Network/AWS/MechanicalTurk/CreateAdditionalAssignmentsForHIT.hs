{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @CreateAdditionalAssignmentsForHIT@ operation increases the maximum number of assignments of an existing HIT.
--
-- To extend the maximum number of assignments, specify the number of additional assignments.
module Network.AWS.MechanicalTurk.CreateAdditionalAssignmentsForHIT
  ( -- * Creating a request
    CreateAdditionalAssignmentsForHIT (..),
    mkCreateAdditionalAssignmentsForHIT,

    -- ** Request lenses
    caafhitHITId,
    caafhitNumberOfAdditionalAssignments,
    caafhitUniqueRequestToken,

    -- * Destructuring the response
    CreateAdditionalAssignmentsForHITResponse (..),
    mkCreateAdditionalAssignmentsForHITResponse,

    -- ** Response lenses
    caafhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateAdditionalAssignmentsForHIT' smart constructor.
data CreateAdditionalAssignmentsForHIT = CreateAdditionalAssignmentsForHIT'
  { -- | The ID of the HIT to extend.
    hITId :: Types.HITId,
    -- | The number of additional assignments to request for this HIT.
    numberOfAdditionalAssignments :: Core.Int,
    -- | A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
    uniqueRequestToken :: Core.Maybe Types.IdempotencyToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAdditionalAssignmentsForHIT' value with any optional fields omitted.
mkCreateAdditionalAssignmentsForHIT ::
  -- | 'hITId'
  Types.HITId ->
  -- | 'numberOfAdditionalAssignments'
  Core.Int ->
  CreateAdditionalAssignmentsForHIT
mkCreateAdditionalAssignmentsForHIT
  hITId
  numberOfAdditionalAssignments =
    CreateAdditionalAssignmentsForHIT'
      { hITId,
        numberOfAdditionalAssignments,
        uniqueRequestToken = Core.Nothing
      }

-- | The ID of the HIT to extend.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitHITId :: Lens.Lens' CreateAdditionalAssignmentsForHIT Types.HITId
caafhitHITId = Lens.field @"hITId"
{-# DEPRECATED caafhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The number of additional assignments to request for this HIT.
--
-- /Note:/ Consider using 'numberOfAdditionalAssignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitNumberOfAdditionalAssignments :: Lens.Lens' CreateAdditionalAssignmentsForHIT Core.Int
caafhitNumberOfAdditionalAssignments = Lens.field @"numberOfAdditionalAssignments"
{-# DEPRECATED caafhitNumberOfAdditionalAssignments "Use generic-lens or generic-optics with 'numberOfAdditionalAssignments' instead." #-}

-- | A unique identifier for this request, which allows you to retry the call on error without extending the HIT multiple times. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the extend HIT already exists in the system from a previous call using the same @UniqueRequestToken@ , subsequent calls will return an error with a message containing the request ID.
--
-- /Note:/ Consider using 'uniqueRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitUniqueRequestToken :: Lens.Lens' CreateAdditionalAssignmentsForHIT (Core.Maybe Types.IdempotencyToken)
caafhitUniqueRequestToken = Lens.field @"uniqueRequestToken"
{-# DEPRECATED caafhitUniqueRequestToken "Use generic-lens or generic-optics with 'uniqueRequestToken' instead." #-}

instance Core.FromJSON CreateAdditionalAssignmentsForHIT where
  toJSON CreateAdditionalAssignmentsForHIT {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            Core.Just
              ( "NumberOfAdditionalAssignments"
                  Core..= numberOfAdditionalAssignments
              ),
            ("UniqueRequestToken" Core..=) Core.<$> uniqueRequestToken
          ]
      )

instance Core.AWSRequest CreateAdditionalAssignmentsForHIT where
  type
    Rs CreateAdditionalAssignmentsForHIT =
      CreateAdditionalAssignmentsForHITResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.CreateAdditionalAssignmentsForHIT"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateAdditionalAssignmentsForHITResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAdditionalAssignmentsForHITResponse' smart constructor.
newtype CreateAdditionalAssignmentsForHITResponse = CreateAdditionalAssignmentsForHITResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAdditionalAssignmentsForHITResponse' value with any optional fields omitted.
mkCreateAdditionalAssignmentsForHITResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAdditionalAssignmentsForHITResponse
mkCreateAdditionalAssignmentsForHITResponse responseStatus =
  CreateAdditionalAssignmentsForHITResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caafhitrrsResponseStatus :: Lens.Lens' CreateAdditionalAssignmentsForHITResponse Core.Int
caafhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED caafhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
