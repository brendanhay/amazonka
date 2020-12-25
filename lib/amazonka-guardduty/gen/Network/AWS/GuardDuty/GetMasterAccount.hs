{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMasterAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details for the GuardDuty master account associated with the current GuardDuty member account.
module Network.AWS.GuardDuty.GetMasterAccount
  ( -- * Creating a request
    GetMasterAccount (..),
    mkGetMasterAccount,

    -- ** Request lenses
    gmaDetectorId,

    -- * Destructuring the response
    GetMasterAccountResponse (..),
    mkGetMasterAccountResponse,

    -- ** Response lenses
    gmarrsMaster,
    gmarrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMasterAccount' smart constructor.
newtype GetMasterAccount = GetMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Types.DetectorId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetMasterAccount' value with any optional fields omitted.
mkGetMasterAccount ::
  -- | 'detectorId'
  Types.DetectorId ->
  GetMasterAccount
mkGetMasterAccount detectorId = GetMasterAccount' {detectorId}

-- | The unique ID of the detector of the GuardDuty member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmaDetectorId :: Lens.Lens' GetMasterAccount Types.DetectorId
gmaDetectorId = Lens.field @"detectorId"
{-# DEPRECATED gmaDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Core.AWSRequest GetMasterAccount where
  type Rs GetMasterAccount = GetMasterAccountResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/master")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMasterAccountResponse'
            Core.<$> (x Core..: "master") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMasterAccountResponse' smart constructor.
data GetMasterAccountResponse = GetMasterAccountResponse'
  { -- | The master account details.
    master :: Types.Master,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMasterAccountResponse' value with any optional fields omitted.
mkGetMasterAccountResponse ::
  -- | 'master'
  Types.Master ->
  -- | 'responseStatus'
  Core.Int ->
  GetMasterAccountResponse
mkGetMasterAccountResponse master responseStatus =
  GetMasterAccountResponse' {master, responseStatus}

-- | The master account details.
--
-- /Note:/ Consider using 'master' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmarrsMaster :: Lens.Lens' GetMasterAccountResponse Types.Master
gmarrsMaster = Lens.field @"master"
{-# DEPRECATED gmarrsMaster "Use generic-lens or generic-optics with 'master' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmarrsResponseStatus :: Lens.Lens' GetMasterAccountResponse Core.Int
gmarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
