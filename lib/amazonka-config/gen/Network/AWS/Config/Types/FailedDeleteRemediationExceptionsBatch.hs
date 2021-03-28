{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
  ( FailedDeleteRemediationExceptionsBatch (..)
  -- * Smart constructor
  , mkFailedDeleteRemediationExceptionsBatch
  -- * Lenses
  , fdrebFailedItems
  , fdrebFailureMessage
  ) where

import qualified Network.AWS.Config.Types.RemediationExceptionResourceKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of each of the failed delete remediation exceptions with specific reasons.
--
-- /See:/ 'mkFailedDeleteRemediationExceptionsBatch' smart constructor.
data FailedDeleteRemediationExceptionsBatch = FailedDeleteRemediationExceptionsBatch'
  { failedItems :: Core.Maybe (Core.NonEmpty Types.RemediationExceptionResourceKey)
    -- ^ Returns remediation exception resource key object of the failed items.
  , failureMessage :: Core.Maybe Core.Text
    -- ^ Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedDeleteRemediationExceptionsBatch' value with any optional fields omitted.
mkFailedDeleteRemediationExceptionsBatch
    :: FailedDeleteRemediationExceptionsBatch
mkFailedDeleteRemediationExceptionsBatch
  = FailedDeleteRemediationExceptionsBatch'{failedItems =
                                              Core.Nothing,
                                            failureMessage = Core.Nothing}

-- | Returns remediation exception resource key object of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdrebFailedItems :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Core.Maybe (Core.NonEmpty Types.RemediationExceptionResourceKey))
fdrebFailedItems = Lens.field @"failedItems"
{-# INLINEABLE fdrebFailedItems #-}
{-# DEPRECATED failedItems "Use generic-lens or generic-optics with 'failedItems' instead"  #-}

-- | Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdrebFailureMessage :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Core.Maybe Core.Text)
fdrebFailureMessage = Lens.field @"failureMessage"
{-# INLINEABLE fdrebFailureMessage #-}
{-# DEPRECATED failureMessage "Use generic-lens or generic-optics with 'failureMessage' instead"  #-}

instance Core.FromJSON FailedDeleteRemediationExceptionsBatch where
        parseJSON
          = Core.withObject "FailedDeleteRemediationExceptionsBatch" Core.$
              \ x ->
                FailedDeleteRemediationExceptionsBatch' Core.<$>
                  (x Core..:? "FailedItems") Core.<*> x Core..:? "FailureMessage"
