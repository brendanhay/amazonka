{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DeleteMembers
    (
    -- * Creating a request
      DeleteMembers (..)
    , mkDeleteMembers
    -- ** Request lenses
    , dmDetectorId
    , dmAccountIds

    -- * Destructuring the response
    , DeleteMembersResponse (..)
    , mkDeleteMembersResponse
    -- ** Response lenses
    , drsUnprocessedAccounts
    , drsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteMembers' smart constructor.
data DeleteMembers = DeleteMembers'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector of the GuardDuty account whose members you want to delete.
  , accountIds :: Core.NonEmpty Types.AccountId
    -- ^ A list of account IDs of the GuardDuty member accounts that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMembers' value with any optional fields omitted.
mkDeleteMembers
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.NonEmpty Types.AccountId -- ^ 'accountIds'
    -> DeleteMembers
mkDeleteMembers detectorId accountIds
  = DeleteMembers'{detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty account whose members you want to delete.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmDetectorId :: Lens.Lens' DeleteMembers Types.DetectorId
dmDetectorId = Lens.field @"detectorId"
{-# INLINEABLE dmDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to delete.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmAccountIds :: Lens.Lens' DeleteMembers (Core.NonEmpty Types.AccountId)
dmAccountIds = Lens.field @"accountIds"
{-# INLINEABLE dmAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

instance Core.ToQuery DeleteMembers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteMembers where
        toHeaders DeleteMembers{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteMembers where
        toJSON DeleteMembers{..}
          = Core.object
              (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest DeleteMembers where
        type Rs DeleteMembers = DeleteMembersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/member/delete",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteMembersResponse' Core.<$>
                   (x Core..:? "unprocessedAccounts" Core..!= Core.mempty) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteMembersResponse' smart constructor.
data DeleteMembersResponse = DeleteMembersResponse'
  { unprocessedAccounts :: [Types.UnprocessedAccount]
    -- ^ The accounts that could not be processed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMembersResponse' value with any optional fields omitted.
mkDeleteMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteMembersResponse
mkDeleteMembersResponse responseStatus
  = DeleteMembersResponse'{unprocessedAccounts = Core.mempty,
                           responseStatus}

-- | The accounts that could not be processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsUnprocessedAccounts :: Lens.Lens' DeleteMembersResponse [Types.UnprocessedAccount]
drsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# INLINEABLE drsUnprocessedAccounts #-}
{-# DEPRECATED unprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteMembersResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
