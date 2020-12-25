{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DisassociateFromMasterAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the current GuardDuty member account from its master account.
module Network.AWS.GuardDuty.DisassociateFromMasterAccount
  ( -- * Creating a request
    DisassociateFromMasterAccount (..),
    mkDisassociateFromMasterAccount,

    -- ** Request lenses
    dfmaDetectorId,

    -- * Destructuring the response
    DisassociateFromMasterAccountResponse (..),
    mkDisassociateFromMasterAccountResponse,

    -- ** Response lenses
    dfmarrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisassociateFromMasterAccount' smart constructor.
newtype DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Types.DetectorId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFromMasterAccount' value with any optional fields omitted.
mkDisassociateFromMasterAccount ::
  -- | 'detectorId'
  Types.DetectorId ->
  DisassociateFromMasterAccount
mkDisassociateFromMasterAccount detectorId =
  DisassociateFromMasterAccount' {detectorId}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfmaDetectorId :: Lens.Lens' DisassociateFromMasterAccount Types.DetectorId
dfmaDetectorId = Lens.field @"detectorId"
{-# DEPRECATED dfmaDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Core.FromJSON DisassociateFromMasterAccount where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DisassociateFromMasterAccount where
  type
    Rs DisassociateFromMasterAccount =
      DisassociateFromMasterAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/master/disassociate")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFromMasterAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDisassociateFromMasterAccountResponse' smart constructor.
newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateFromMasterAccountResponse' value with any optional fields omitted.
mkDisassociateFromMasterAccountResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisassociateFromMasterAccountResponse
mkDisassociateFromMasterAccountResponse responseStatus =
  DisassociateFromMasterAccountResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfmarrsResponseStatus :: Lens.Lens' DisassociateFromMasterAccountResponse Core.Int
dfmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dfmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
