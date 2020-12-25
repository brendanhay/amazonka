{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationBatch
  ( FailedRemediationBatch (..),

    -- * Smart constructor
    mkFailedRemediationBatch,

    -- * Lenses
    frbFailedItems,
    frbFailureMessage,
  )
where

import qualified Network.AWS.Config.Types.RemediationConfiguration as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'mkFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { -- | Returns remediation configurations of the failed items.
    failedItems :: Core.Maybe [Types.RemediationConfiguration],
    -- | Returns a failure message. For example, the resource is already compliant.
    failureMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedRemediationBatch' value with any optional fields omitted.
mkFailedRemediationBatch ::
  FailedRemediationBatch
mkFailedRemediationBatch =
  FailedRemediationBatch'
    { failedItems = Core.Nothing,
      failureMessage = Core.Nothing
    }

-- | Returns remediation configurations of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frbFailedItems :: Lens.Lens' FailedRemediationBatch (Core.Maybe [Types.RemediationConfiguration])
frbFailedItems = Lens.field @"failedItems"
{-# DEPRECATED frbFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

-- | Returns a failure message. For example, the resource is already compliant.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frbFailureMessage :: Lens.Lens' FailedRemediationBatch (Core.Maybe Types.String)
frbFailureMessage = Lens.field @"failureMessage"
{-# DEPRECATED frbFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

instance Core.FromJSON FailedRemediationBatch where
  parseJSON =
    Core.withObject "FailedRemediationBatch" Core.$
      \x ->
        FailedRemediationBatch'
          Core.<$> (x Core..:? "FailedItems") Core.<*> (x Core..:? "FailureMessage")
