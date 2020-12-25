{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationExceptionBatch
  ( FailedRemediationExceptionBatch (..),

    -- * Smart constructor
    mkFailedRemediationExceptionBatch,

    -- * Lenses
    frebFailedItems,
    frebFailureMessage,
  )
where

import qualified Network.AWS.Config.Types.FailureMessage as Types
import qualified Network.AWS.Config.Types.RemediationException as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of each of the failed remediation exceptions with specific reasons.
--
-- /See:/ 'mkFailedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Core.Maybe [Types.RemediationException],
    -- | Returns a failure message. For example, the auto-remediation has failed.
    failureMessage :: Core.Maybe Types.FailureMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FailedRemediationExceptionBatch' value with any optional fields omitted.
mkFailedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
mkFailedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { failedItems = Core.Nothing,
      failureMessage = Core.Nothing
    }

-- | Returns remediation exception resource key object of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frebFailedItems :: Lens.Lens' FailedRemediationExceptionBatch (Core.Maybe [Types.RemediationException])
frebFailedItems = Lens.field @"failedItems"
{-# DEPRECATED frebFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | Returns a failure message. For example, the auto-remediation has failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frebFailureMessage :: Lens.Lens' FailedRemediationExceptionBatch (Core.Maybe Types.FailureMessage)
frebFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED frebFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

instance Core.FromJSON FailedRemediationExceptionBatch where
  parseJSON =
    Core.withObject "FailedRemediationExceptionBatch" Core.$
      \x ->
        FailedRemediationExceptionBatch'
          Core.<$> (x Core..:? "FailedItems") Core.<*> (x Core..:? "FailureMessage")
