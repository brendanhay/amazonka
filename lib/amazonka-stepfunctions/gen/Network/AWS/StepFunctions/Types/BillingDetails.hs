{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.BillingDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.BillingDetails
  ( BillingDetails (..),

    -- * Smart constructor
    mkBillingDetails,

    -- * Lenses
    bdBilledMemoryUsedInMB,
    bdBilledDurationInMilliseconds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that describes workflow billing details.
--
-- /See:/ 'mkBillingDetails' smart constructor.
data BillingDetails = BillingDetails'
  { -- | Billed memory consumption of your workflow, in MB.
    billedMemoryUsedInMB :: Lude.Maybe Lude.Natural,
    -- | Billed duration of your workflow, in milliseconds.
    billedDurationInMilliseconds :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BillingDetails' with the minimum fields required to make a request.
--
-- * 'billedMemoryUsedInMB' - Billed memory consumption of your workflow, in MB.
-- * 'billedDurationInMilliseconds' - Billed duration of your workflow, in milliseconds.
mkBillingDetails ::
  BillingDetails
mkBillingDetails =
  BillingDetails'
    { billedMemoryUsedInMB = Lude.Nothing,
      billedDurationInMilliseconds = Lude.Nothing
    }

-- | Billed memory consumption of your workflow, in MB.
--
-- /Note:/ Consider using 'billedMemoryUsedInMB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBilledMemoryUsedInMB :: Lens.Lens' BillingDetails (Lude.Maybe Lude.Natural)
bdBilledMemoryUsedInMB = Lens.lens (billedMemoryUsedInMB :: BillingDetails -> Lude.Maybe Lude.Natural) (\s a -> s {billedMemoryUsedInMB = a} :: BillingDetails)
{-# DEPRECATED bdBilledMemoryUsedInMB "Use generic-lens or generic-optics with 'billedMemoryUsedInMB' instead." #-}

-- | Billed duration of your workflow, in milliseconds.
--
-- /Note:/ Consider using 'billedDurationInMilliseconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdBilledDurationInMilliseconds :: Lens.Lens' BillingDetails (Lude.Maybe Lude.Natural)
bdBilledDurationInMilliseconds = Lens.lens (billedDurationInMilliseconds :: BillingDetails -> Lude.Maybe Lude.Natural) (\s a -> s {billedDurationInMilliseconds = a} :: BillingDetails)
{-# DEPRECATED bdBilledDurationInMilliseconds "Use generic-lens or generic-optics with 'billedDurationInMilliseconds' instead." #-}

instance Lude.FromJSON BillingDetails where
  parseJSON =
    Lude.withObject
      "BillingDetails"
      ( \x ->
          BillingDetails'
            Lude.<$> (x Lude..:? "billedMemoryUsedInMB")
            Lude.<*> (x Lude..:? "billedDurationInMilliseconds")
      )
