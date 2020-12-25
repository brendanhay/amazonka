{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.
--
-- This action verifies a trust relationship between your AWS Managed Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.VerifyTrust
  ( -- * Creating a request
    VerifyTrust (..),
    mkVerifyTrust,

    -- ** Request lenses
    vtTrustId,

    -- * Destructuring the response
    VerifyTrustResponse (..),
    mkVerifyTrustResponse,

    -- ** Response lenses
    vtrrsTrustId,
    vtrrsResponseStatus,
  )
where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the verification of an existing trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkVerifyTrust' smart constructor.
newtype VerifyTrust = VerifyTrust'
  { -- | The unique Trust ID of the trust relationship to verify.
    trustId :: Types.TrustId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyTrust' value with any optional fields omitted.
mkVerifyTrust ::
  -- | 'trustId'
  Types.TrustId ->
  VerifyTrust
mkVerifyTrust trustId = VerifyTrust' {trustId}

-- | The unique Trust ID of the trust relationship to verify.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTrustId :: Lens.Lens' VerifyTrust Types.TrustId
vtTrustId = Lens.field @"trustId"
{-# DEPRECATED vtTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

instance Core.FromJSON VerifyTrust where
  toJSON VerifyTrust {..} =
    Core.object
      (Core.catMaybes [Core.Just ("TrustId" Core..= trustId)])

instance Core.AWSRequest VerifyTrust where
  type Rs VerifyTrust = VerifyTrustResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DirectoryService_20150416.VerifyTrust")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyTrustResponse'
            Core.<$> (x Core..:? "TrustId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Result of a VerifyTrust request.
--
-- /See:/ 'mkVerifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { -- | The unique Trust ID of the trust relationship that was verified.
    trustId :: Core.Maybe Types.TrustId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifyTrustResponse' value with any optional fields omitted.
mkVerifyTrustResponse ::
  -- | 'responseStatus'
  Core.Int ->
  VerifyTrustResponse
mkVerifyTrustResponse responseStatus =
  VerifyTrustResponse' {trustId = Core.Nothing, responseStatus}

-- | The unique Trust ID of the trust relationship that was verified.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsTrustId :: Lens.Lens' VerifyTrustResponse (Core.Maybe Types.TrustId)
vtrrsTrustId = Lens.field @"trustId"
{-# DEPRECATED vtrrsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsResponseStatus :: Lens.Lens' VerifyTrustResponse Core.Int
vtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
