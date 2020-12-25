{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateMemberDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information on member accounts to be updated.
module Network.AWS.GuardDuty.UpdateMemberDetectors
  ( -- * Creating a request
    UpdateMemberDetectors (..),
    mkUpdateMemberDetectors,

    -- ** Request lenses
    umdDetectorId,
    umdAccountIds,
    umdDataSources,

    -- * Destructuring the response
    UpdateMemberDetectorsResponse (..),
    mkUpdateMemberDetectorsResponse,

    -- ** Response lenses
    umdrrsUnprocessedAccounts,
    umdrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateMemberDetectors' smart constructor.
data UpdateMemberDetectors = UpdateMemberDetectors'
  { -- | The detector ID of the master account.
    detectorId :: Types.DetectorId,
    -- | A list of member account IDs to be updated.
    accountIds :: Core.NonEmpty Types.AccountId,
    -- | An object describes which data sources will be updated.
    dataSources :: Core.Maybe Types.DataSourceConfigurations
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMemberDetectors' value with any optional fields omitted.
mkUpdateMemberDetectors ::
  -- | 'detectorId'
  Types.DetectorId ->
  -- | 'accountIds'
  Core.NonEmpty Types.AccountId ->
  UpdateMemberDetectors
mkUpdateMemberDetectors detectorId accountIds =
  UpdateMemberDetectors'
    { detectorId,
      accountIds,
      dataSources = Core.Nothing
    }

-- | The detector ID of the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdDetectorId :: Lens.Lens' UpdateMemberDetectors Types.DetectorId
umdDetectorId = Lens.field @"detectorId"
{-# DEPRECATED umdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of member account IDs to be updated.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdAccountIds :: Lens.Lens' UpdateMemberDetectors (Core.NonEmpty Types.AccountId)
umdAccountIds = Lens.field @"accountIds"
{-# DEPRECATED umdAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | An object describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdDataSources :: Lens.Lens' UpdateMemberDetectors (Core.Maybe Types.DataSourceConfigurations)
umdDataSources = Lens.field @"dataSources"
{-# DEPRECATED umdDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

instance Core.FromJSON UpdateMemberDetectors where
  toJSON UpdateMemberDetectors {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("accountIds" Core..= accountIds),
            ("dataSources" Core..=) Core.<$> dataSources
          ]
      )

instance Core.AWSRequest UpdateMemberDetectors where
  type Rs UpdateMemberDetectors = UpdateMemberDetectorsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/member/detector/update")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMemberDetectorsResponse'
            Core.<$> (x Core..:? "unprocessedAccounts" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateMemberDetectorsResponse' smart constructor.
data UpdateMemberDetectorsResponse = UpdateMemberDetectorsResponse'
  { -- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
    unprocessedAccounts :: [Types.UnprocessedAccount],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMemberDetectorsResponse' value with any optional fields omitted.
mkUpdateMemberDetectorsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateMemberDetectorsResponse
mkUpdateMemberDetectorsResponse responseStatus =
  UpdateMemberDetectorsResponse'
    { unprocessedAccounts = Core.mempty,
      responseStatus
    }

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdrrsUnprocessedAccounts :: Lens.Lens' UpdateMemberDetectorsResponse [Types.UnprocessedAccount]
umdrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# DEPRECATED umdrrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdrrsResponseStatus :: Lens.Lens' UpdateMemberDetectorsResponse Core.Int
umdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED umdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
