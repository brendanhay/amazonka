{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.ResolveCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resolves a support case. This operation takes a @caseId@ and returns the initial and final state of the case.
module Network.AWS.Support.ResolveCase
  ( -- * Creating a request
    ResolveCase (..),
    mkResolveCase,

    -- ** Request lenses
    rcCaseId,

    -- * Destructuring the response
    ResolveCaseResponse (..),
    mkResolveCaseResponse,

    -- ** Response lenses
    rcrrsFinalCaseStatus,
    rcrrsInitialCaseStatus,
    rcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkResolveCase' smart constructor.
newtype ResolveCase = ResolveCase'
  { -- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Core.Maybe Types.CaseId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveCase' value with any optional fields omitted.
mkResolveCase ::
  ResolveCase
mkResolveCase = ResolveCase' {caseId = Core.Nothing}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCaseId :: Lens.Lens' ResolveCase (Core.Maybe Types.CaseId)
rcCaseId = Lens.field @"caseId"
{-# DEPRECATED rcCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

instance Core.FromJSON ResolveCase where
  toJSON ResolveCase {..} =
    Core.object (Core.catMaybes [("caseId" Core..=) Core.<$> caseId])

instance Core.AWSRequest ResolveCase where
  type Rs ResolveCase = ResolveCaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSSupport_20130415.ResolveCase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveCaseResponse'
            Core.<$> (x Core..:? "finalCaseStatus")
            Core.<*> (x Core..:? "initialCaseStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The status of the case returned by the 'ResolveCase' operation.
--
-- /See:/ 'mkResolveCaseResponse' smart constructor.
data ResolveCaseResponse = ResolveCaseResponse'
  { -- | The status of the case after the 'ResolveCase' request was processed.
    finalCaseStatus :: Core.Maybe Types.FinalCaseStatus,
    -- | The status of the case when the 'ResolveCase' request was sent.
    initialCaseStatus :: Core.Maybe Types.InitialCaseStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolveCaseResponse' value with any optional fields omitted.
mkResolveCaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ResolveCaseResponse
mkResolveCaseResponse responseStatus =
  ResolveCaseResponse'
    { finalCaseStatus = Core.Nothing,
      initialCaseStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the case after the 'ResolveCase' request was processed.
--
-- /Note:/ Consider using 'finalCaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsFinalCaseStatus :: Lens.Lens' ResolveCaseResponse (Core.Maybe Types.FinalCaseStatus)
rcrrsFinalCaseStatus = Lens.field @"finalCaseStatus"
{-# DEPRECATED rcrrsFinalCaseStatus "Use generic-lens or generic-optics with 'finalCaseStatus' instead." #-}

-- | The status of the case when the 'ResolveCase' request was sent.
--
-- /Note:/ Consider using 'initialCaseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsInitialCaseStatus :: Lens.Lens' ResolveCaseResponse (Core.Maybe Types.InitialCaseStatus)
rcrrsInitialCaseStatus = Lens.field @"initialCaseStatus"
{-# DEPRECATED rcrrsInitialCaseStatus "Use generic-lens or generic-optics with 'initialCaseStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' ResolveCaseResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
