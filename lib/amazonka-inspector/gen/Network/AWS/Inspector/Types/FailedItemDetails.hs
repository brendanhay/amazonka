{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FailedItemDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.FailedItemDetails
  ( FailedItemDetails (..)
  -- * Smart constructor
  , mkFailedItemDetails
  -- * Lenses
  , fidFailureCode
  , fidRetryable
  ) where

import qualified Network.AWS.Inspector.Types.FailedItemErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Includes details about the failed items.
--
-- /See:/ 'mkFailedItemDetails' smart constructor.
data FailedItemDetails = FailedItemDetails'
  { failureCode :: Types.FailedItemErrorCode
    -- ^ The status code of a failed item.
  , retryable :: Core.Bool
    -- ^ Indicates whether you can immediately retry a request for this item for a specified resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedItemDetails' value with any optional fields omitted.
mkFailedItemDetails
    :: Types.FailedItemErrorCode -- ^ 'failureCode'
    -> Core.Bool -- ^ 'retryable'
    -> FailedItemDetails
mkFailedItemDetails failureCode retryable
  = FailedItemDetails'{failureCode, retryable}

-- | The status code of a failed item.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fidFailureCode :: Lens.Lens' FailedItemDetails Types.FailedItemErrorCode
fidFailureCode = Lens.field @"failureCode"
{-# INLINEABLE fidFailureCode #-}
{-# DEPRECATED failureCode "Use generic-lens or generic-optics with 'failureCode' instead"  #-}

-- | Indicates whether you can immediately retry a request for this item for a specified resource.
--
-- /Note:/ Consider using 'retryable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fidRetryable :: Lens.Lens' FailedItemDetails Core.Bool
fidRetryable = Lens.field @"retryable"
{-# INLINEABLE fidRetryable #-}
{-# DEPRECATED retryable "Use generic-lens or generic-optics with 'retryable' instead"  #-}

instance Core.FromJSON FailedItemDetails where
        parseJSON
          = Core.withObject "FailedItemDetails" Core.$
              \ x ->
                FailedItemDetails' Core.<$>
                  (x Core..: "failureCode") Core.<*> x Core..: "retryable"
