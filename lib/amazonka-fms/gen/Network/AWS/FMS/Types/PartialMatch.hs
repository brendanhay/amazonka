{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PartialMatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PartialMatch
  ( PartialMatch (..),

    -- * Smart constructor
    mkPartialMatch,

    -- * Lenses
    pmTargetViolationReasons,
    pmReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reference rule that partially matches the @ViolationTarget@ rule and violation reason.
--
-- /See:/ 'mkPartialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { targetViolationReasons ::
      Lude.Maybe [Lude.Text],
    reference :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartialMatch' with the minimum fields required to make a request.
--
-- * 'reference' - The reference rule from the master security group of the AWS Firewall Manager policy.
-- * 'targetViolationReasons' - The violation reason.
mkPartialMatch ::
  PartialMatch
mkPartialMatch =
  PartialMatch'
    { targetViolationReasons = Lude.Nothing,
      reference = Lude.Nothing
    }

-- | The violation reason.
--
-- /Note:/ Consider using 'targetViolationReasons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmTargetViolationReasons :: Lens.Lens' PartialMatch (Lude.Maybe [Lude.Text])
pmTargetViolationReasons = Lens.lens (targetViolationReasons :: PartialMatch -> Lude.Maybe [Lude.Text]) (\s a -> s {targetViolationReasons = a} :: PartialMatch)
{-# DEPRECATED pmTargetViolationReasons "Use generic-lens or generic-optics with 'targetViolationReasons' instead." #-}

-- | The reference rule from the master security group of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'reference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmReference :: Lens.Lens' PartialMatch (Lude.Maybe Lude.Text)
pmReference = Lens.lens (reference :: PartialMatch -> Lude.Maybe Lude.Text) (\s a -> s {reference = a} :: PartialMatch)
{-# DEPRECATED pmReference "Use generic-lens or generic-optics with 'reference' instead." #-}

instance Lude.FromJSON PartialMatch where
  parseJSON =
    Lude.withObject
      "PartialMatch"
      ( \x ->
          PartialMatch'
            Lude.<$> (x Lude..:? "TargetViolationReasons" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Reference")
      )
