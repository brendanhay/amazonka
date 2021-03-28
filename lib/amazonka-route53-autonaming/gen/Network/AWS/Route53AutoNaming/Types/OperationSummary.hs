{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53AutoNaming.Types.OperationSummary
  ( OperationSummary (..)
  -- * Smart constructor
  , mkOperationSummary
  -- * Lenses
  , osId
  , osStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.OperationId as Types
import qualified Network.AWS.Route53AutoNaming.Types.OperationStatus as Types

-- | A complex type that contains information about an operation that matches the criteria that you specified in a <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListOperations.html ListOperations> request.
--
-- /See:/ 'mkOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { id :: Core.Maybe Types.OperationId
    -- ^ The ID for an operation.
  , status :: Core.Maybe Types.OperationStatus
    -- ^ The status of the operation. Values include the following:
--
--
--     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
--
--
--     * __PENDING__ : AWS Cloud Map is performing the operation.
--
--
--     * __SUCCESS__ : The operation succeeded.
--
--
--     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OperationSummary' value with any optional fields omitted.
mkOperationSummary
    :: OperationSummary
mkOperationSummary
  = OperationSummary'{id = Core.Nothing, status = Core.Nothing}

-- | The ID for an operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osId :: Lens.Lens' OperationSummary (Core.Maybe Types.OperationId)
osId = Lens.field @"id"
{-# INLINEABLE osId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The status of the operation. Values include the following:
--
--
--     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
--
--
--     * __PENDING__ : AWS Cloud Map is performing the operation.
--
--
--     * __SUCCESS__ : The operation succeeded.
--
--
--     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osStatus :: Lens.Lens' OperationSummary (Core.Maybe Types.OperationStatus)
osStatus = Lens.field @"status"
{-# INLINEABLE osStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON OperationSummary where
        parseJSON
          = Core.withObject "OperationSummary" Core.$
              \ x ->
                OperationSummary' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "Status"
