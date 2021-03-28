{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
    (
    -- * Creating a request
      GetMembers (..)
    , mkGetMembers
    -- ** Request lenses
    , gmDetectorId
    , gmAccountIds

    -- * Destructuring the response
    , GetMembersResponse (..)
    , mkGetMembersResponse
    -- ** Response lenses
    , gmrrsMembers
    , gmrrsUnprocessedAccounts
    , gmrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMembers' smart constructor.
data GetMembers = GetMembers'
  { detectorId :: Types.DetectorId
    -- ^ The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
  , accountIds :: Core.NonEmpty Types.AccountId
    -- ^ A list of account IDs of the GuardDuty member accounts that you want to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMembers' value with any optional fields omitted.
mkGetMembers
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.NonEmpty Types.AccountId -- ^ 'accountIds'
    -> GetMembers
mkGetMembers detectorId accountIds
  = GetMembers'{detectorId, accountIds}

-- | The unique ID of the detector of the GuardDuty account whose members you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmDetectorId :: Lens.Lens' GetMembers Types.DetectorId
gmDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gmDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | A list of account IDs of the GuardDuty member accounts that you want to describe.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmAccountIds :: Lens.Lens' GetMembers (Core.NonEmpty Types.AccountId)
gmAccountIds = Lens.field @"accountIds"
{-# INLINEABLE gmAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

instance Core.ToQuery GetMembers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMembers where
        toHeaders GetMembers{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMembers where
        toJSON GetMembers{..}
          = Core.object
              (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest GetMembers where
        type Rs GetMembers = GetMembersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<> "/member/get",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMembersResponse' Core.<$>
                   (x Core..:? "members" Core..!= Core.mempty) Core.<*>
                     x Core..:? "unprocessedAccounts" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { members :: [Types.Member]
    -- ^ A list of members.
  , unprocessedAccounts :: [Types.UnprocessedAccount]
    -- ^ A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMembersResponse' value with any optional fields omitted.
mkGetMembersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMembersResponse
mkGetMembersResponse responseStatus
  = GetMembersResponse'{members = Core.mempty,
                        unprocessedAccounts = Core.mempty, responseStatus}

-- | A list of members.
--
-- /Note:/ Consider using 'members' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsMembers :: Lens.Lens' GetMembersResponse [Types.Member]
gmrrsMembers = Lens.field @"members"
{-# INLINEABLE gmrrsMembers #-}
{-# DEPRECATED members "Use generic-lens or generic-optics with 'members' instead"  #-}

-- | A list of objects that contain the unprocessed account and a result string that explains why it was unprocessed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsUnprocessedAccounts :: Lens.Lens' GetMembersResponse [Types.UnprocessedAccount]
gmrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# INLINEABLE gmrrsUnprocessedAccounts #-}
{-# DEPRECATED unprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmrrsResponseStatus :: Lens.Lens' GetMembersResponse Core.Int
gmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
