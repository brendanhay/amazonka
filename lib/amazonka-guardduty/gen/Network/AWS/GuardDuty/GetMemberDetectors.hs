{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMemberDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes which data sources are enabled for the member account's detector.
module Network.AWS.GuardDuty.GetMemberDetectors
    (
    -- * Creating a request
      GetMemberDetectors (..)
    , mkGetMemberDetectors
    -- ** Request lenses
    , gmdDetectorId
    , gmdAccountIds

    -- * Destructuring the response
    , GetMemberDetectorsResponse (..)
    , mkGetMemberDetectorsResponse
    -- ** Response lenses
    , gmdrrsMemberDataSourceConfigurations
    , gmdrrsUnprocessedAccounts
    , gmdrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMemberDetectors' smart constructor.
data GetMemberDetectors = GetMemberDetectors'
  { detectorId :: Types.DetectorId
    -- ^ The detector ID for the master account.
  , accountIds :: Core.NonEmpty Types.AccountId
    -- ^ The account ID of the member account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMemberDetectors' value with any optional fields omitted.
mkGetMemberDetectors
    :: Types.DetectorId -- ^ 'detectorId'
    -> Core.NonEmpty Types.AccountId -- ^ 'accountIds'
    -> GetMemberDetectors
mkGetMemberDetectors detectorId accountIds
  = GetMemberDetectors'{detectorId, accountIds}

-- | The detector ID for the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdDetectorId :: Lens.Lens' GetMemberDetectors Types.DetectorId
gmdDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gmdDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The account ID of the member account.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdAccountIds :: Lens.Lens' GetMemberDetectors (Core.NonEmpty Types.AccountId)
gmdAccountIds = Lens.field @"accountIds"
{-# INLINEABLE gmdAccountIds #-}
{-# DEPRECATED accountIds "Use generic-lens or generic-optics with 'accountIds' instead"  #-}

instance Core.ToQuery GetMemberDetectors where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetMemberDetectors where
        toHeaders GetMemberDetectors{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetMemberDetectors where
        toJSON GetMemberDetectors{..}
          = Core.object
              (Core.catMaybes [Core.Just ("accountIds" Core..= accountIds)])

instance Core.AWSRequest GetMemberDetectors where
        type Rs GetMemberDetectors = GetMemberDetectorsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/member/detector/get",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetMemberDetectorsResponse' Core.<$>
                   (x Core..: "members") Core.<*>
                     x Core..:? "unprocessedAccounts" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetMemberDetectorsResponse' smart constructor.
data GetMemberDetectorsResponse = GetMemberDetectorsResponse'
  { memberDataSourceConfigurations :: Core.NonEmpty Types.MemberDataSourceConfiguration
    -- ^ An object that describes which data sources are enabled for a member account.
  , unprocessedAccounts :: [Types.UnprocessedAccount]
    -- ^ A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetMemberDetectorsResponse' value with any optional fields omitted.
mkGetMemberDetectorsResponse
    :: Core.NonEmpty Types.MemberDataSourceConfiguration -- ^ 'memberDataSourceConfigurations'
    -> Core.Int -- ^ 'responseStatus'
    -> GetMemberDetectorsResponse
mkGetMemberDetectorsResponse memberDataSourceConfigurations
  responseStatus
  = GetMemberDetectorsResponse'{memberDataSourceConfigurations,
                                unprocessedAccounts = Core.mempty, responseStatus}

-- | An object that describes which data sources are enabled for a member account.
--
-- /Note:/ Consider using 'memberDataSourceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMemberDataSourceConfigurations :: Lens.Lens' GetMemberDetectorsResponse (Core.NonEmpty Types.MemberDataSourceConfiguration)
gmdrrsMemberDataSourceConfigurations = Lens.field @"memberDataSourceConfigurations"
{-# INLINEABLE gmdrrsMemberDataSourceConfigurations #-}
{-# DEPRECATED memberDataSourceConfigurations "Use generic-lens or generic-optics with 'memberDataSourceConfigurations' instead"  #-}

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsUnprocessedAccounts :: Lens.Lens' GetMemberDetectorsResponse [Types.UnprocessedAccount]
gmdrrsUnprocessedAccounts = Lens.field @"unprocessedAccounts"
{-# INLINEABLE gmdrrsUnprocessedAccounts #-}
{-# DEPRECATED unprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsResponseStatus :: Lens.Lens' GetMemberDetectorsResponse Core.Int
gmdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
