{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.TargetLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.TargetLocation
  ( TargetLocation (..)
  -- * Smart constructor
  , mkTargetLocation
  -- * Lenses
  , tlAccounts
  , tlExecutionRoleName
  , tlRegions
  , tlTargetLocationMaxConcurrency
  , tlTargetLocationMaxErrors
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Account as Types
import qualified Network.AWS.SSM.Types.ExecutionRoleName as Types
import qualified Network.AWS.SSM.Types.Region as Types
import qualified Network.AWS.SSM.Types.TargetLocationMaxConcurrency as Types
import qualified Network.AWS.SSM.Types.TargetLocationMaxErrors as Types

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- /See:/ 'mkTargetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { accounts :: Core.Maybe (Core.NonEmpty Types.Account)
    -- ^ The AWS accounts targeted by the current Automation execution.
  , executionRoleName :: Core.Maybe Types.ExecutionRoleName
    -- ^ The Automation execution role used by the currently running Automation.
  , regions :: Core.Maybe (Core.NonEmpty Types.Region)
    -- ^ The AWS Regions targeted by the current Automation execution.
  , targetLocationMaxConcurrency :: Core.Maybe Types.TargetLocationMaxConcurrency
    -- ^ The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently 
  , targetLocationMaxErrors :: Core.Maybe Types.TargetLocationMaxErrors
    -- ^ The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetLocation' value with any optional fields omitted.
mkTargetLocation
    :: TargetLocation
mkTargetLocation
  = TargetLocation'{accounts = Core.Nothing,
                    executionRoleName = Core.Nothing, regions = Core.Nothing,
                    targetLocationMaxConcurrency = Core.Nothing,
                    targetLocationMaxErrors = Core.Nothing}

-- | The AWS accounts targeted by the current Automation execution.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlAccounts :: Lens.Lens' TargetLocation (Core.Maybe (Core.NonEmpty Types.Account))
tlAccounts = Lens.field @"accounts"
{-# INLINEABLE tlAccounts #-}
{-# DEPRECATED accounts "Use generic-lens or generic-optics with 'accounts' instead"  #-}

-- | The Automation execution role used by the currently running Automation.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlExecutionRoleName :: Lens.Lens' TargetLocation (Core.Maybe Types.ExecutionRoleName)
tlExecutionRoleName = Lens.field @"executionRoleName"
{-# INLINEABLE tlExecutionRoleName #-}
{-# DEPRECATED executionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead"  #-}

-- | The AWS Regions targeted by the current Automation execution.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlRegions :: Lens.Lens' TargetLocation (Core.Maybe (Core.NonEmpty Types.Region))
tlRegions = Lens.field @"regions"
{-# INLINEABLE tlRegions #-}
{-# DEPRECATED regions "Use generic-lens or generic-optics with 'regions' instead"  #-}

-- | The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently 
--
-- /Note:/ Consider using 'targetLocationMaxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlTargetLocationMaxConcurrency :: Lens.Lens' TargetLocation (Core.Maybe Types.TargetLocationMaxConcurrency)
tlTargetLocationMaxConcurrency = Lens.field @"targetLocationMaxConcurrency"
{-# INLINEABLE tlTargetLocationMaxConcurrency #-}
{-# DEPRECATED targetLocationMaxConcurrency "Use generic-lens or generic-optics with 'targetLocationMaxConcurrency' instead"  #-}

-- | The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation. 
--
-- /Note:/ Consider using 'targetLocationMaxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlTargetLocationMaxErrors :: Lens.Lens' TargetLocation (Core.Maybe Types.TargetLocationMaxErrors)
tlTargetLocationMaxErrors = Lens.field @"targetLocationMaxErrors"
{-# INLINEABLE tlTargetLocationMaxErrors #-}
{-# DEPRECATED targetLocationMaxErrors "Use generic-lens or generic-optics with 'targetLocationMaxErrors' instead"  #-}

instance Core.FromJSON TargetLocation where
        toJSON TargetLocation{..}
          = Core.object
              (Core.catMaybes
                 [("Accounts" Core..=) Core.<$> accounts,
                  ("ExecutionRoleName" Core..=) Core.<$> executionRoleName,
                  ("Regions" Core..=) Core.<$> regions,
                  ("TargetLocationMaxConcurrency" Core..=) Core.<$>
                    targetLocationMaxConcurrency,
                  ("TargetLocationMaxErrors" Core..=) Core.<$>
                    targetLocationMaxErrors])

instance Core.FromJSON TargetLocation where
        parseJSON
          = Core.withObject "TargetLocation" Core.$
              \ x ->
                TargetLocation' Core.<$>
                  (x Core..:? "Accounts") Core.<*> x Core..:? "ExecutionRoleName"
                    Core.<*> x Core..:? "Regions"
                    Core.<*> x Core..:? "TargetLocationMaxConcurrency"
                    Core.<*> x Core..:? "TargetLocationMaxErrors"
