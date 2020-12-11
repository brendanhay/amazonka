-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.TargetLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.TargetLocation
  ( TargetLocation (..),

    -- * Smart constructor
    mkTargetLocation,

    -- * Lenses
    tlAccounts,
    tlTargetLocationMaxConcurrency,
    tlTargetLocationMaxErrors,
    tlRegions,
    tlExecutionRoleName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The combination of AWS Regions and accounts targeted by the current Automation execution.
--
-- /See:/ 'mkTargetLocation' smart constructor.
data TargetLocation = TargetLocation'
  { accounts ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    targetLocationMaxConcurrency :: Lude.Maybe Lude.Text,
    targetLocationMaxErrors :: Lude.Maybe Lude.Text,
    regions :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    executionRoleName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetLocation' with the minimum fields required to make a request.
--
-- * 'accounts' - The AWS accounts targeted by the current Automation execution.
-- * 'executionRoleName' - The Automation execution role used by the currently running Automation.
-- * 'regions' - The AWS Regions targeted by the current Automation execution.
-- * 'targetLocationMaxConcurrency' - The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently
-- * 'targetLocationMaxErrors' - The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation.
mkTargetLocation ::
  TargetLocation
mkTargetLocation =
  TargetLocation'
    { accounts = Lude.Nothing,
      targetLocationMaxConcurrency = Lude.Nothing,
      targetLocationMaxErrors = Lude.Nothing,
      regions = Lude.Nothing,
      executionRoleName = Lude.Nothing
    }

-- | The AWS accounts targeted by the current Automation execution.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlAccounts :: Lens.Lens' TargetLocation (Lude.Maybe (Lude.NonEmpty Lude.Text))
tlAccounts = Lens.lens (accounts :: TargetLocation -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {accounts = a} :: TargetLocation)
{-# DEPRECATED tlAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | The maximum number of AWS accounts and AWS regions allowed to run the Automation concurrently
--
-- /Note:/ Consider using 'targetLocationMaxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlTargetLocationMaxConcurrency :: Lens.Lens' TargetLocation (Lude.Maybe Lude.Text)
tlTargetLocationMaxConcurrency = Lens.lens (targetLocationMaxConcurrency :: TargetLocation -> Lude.Maybe Lude.Text) (\s a -> s {targetLocationMaxConcurrency = a} :: TargetLocation)
{-# DEPRECATED tlTargetLocationMaxConcurrency "Use generic-lens or generic-optics with 'targetLocationMaxConcurrency' instead." #-}

-- | The maximum number of errors allowed before the system stops queueing additional Automation executions for the currently running Automation.
--
-- /Note:/ Consider using 'targetLocationMaxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlTargetLocationMaxErrors :: Lens.Lens' TargetLocation (Lude.Maybe Lude.Text)
tlTargetLocationMaxErrors = Lens.lens (targetLocationMaxErrors :: TargetLocation -> Lude.Maybe Lude.Text) (\s a -> s {targetLocationMaxErrors = a} :: TargetLocation)
{-# DEPRECATED tlTargetLocationMaxErrors "Use generic-lens or generic-optics with 'targetLocationMaxErrors' instead." #-}

-- | The AWS Regions targeted by the current Automation execution.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlRegions :: Lens.Lens' TargetLocation (Lude.Maybe (Lude.NonEmpty Lude.Text))
tlRegions = Lens.lens (regions :: TargetLocation -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {regions = a} :: TargetLocation)
{-# DEPRECATED tlRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The Automation execution role used by the currently running Automation.
--
-- /Note:/ Consider using 'executionRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlExecutionRoleName :: Lens.Lens' TargetLocation (Lude.Maybe Lude.Text)
tlExecutionRoleName = Lens.lens (executionRoleName :: TargetLocation -> Lude.Maybe Lude.Text) (\s a -> s {executionRoleName = a} :: TargetLocation)
{-# DEPRECATED tlExecutionRoleName "Use generic-lens or generic-optics with 'executionRoleName' instead." #-}

instance Lude.FromJSON TargetLocation where
  parseJSON =
    Lude.withObject
      "TargetLocation"
      ( \x ->
          TargetLocation'
            Lude.<$> (x Lude..:? "Accounts")
            Lude.<*> (x Lude..:? "TargetLocationMaxConcurrency")
            Lude.<*> (x Lude..:? "TargetLocationMaxErrors")
            Lude.<*> (x Lude..:? "Regions")
            Lude.<*> (x Lude..:? "ExecutionRoleName")
      )

instance Lude.ToJSON TargetLocation where
  toJSON TargetLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Accounts" Lude..=) Lude.<$> accounts,
            ("TargetLocationMaxConcurrency" Lude..=)
              Lude.<$> targetLocationMaxConcurrency,
            ("TargetLocationMaxErrors" Lude..=)
              Lude.<$> targetLocationMaxErrors,
            ("Regions" Lude..=) Lude.<$> regions,
            ("ExecutionRoleName" Lude..=) Lude.<$> executionRoleName
          ]
      )
