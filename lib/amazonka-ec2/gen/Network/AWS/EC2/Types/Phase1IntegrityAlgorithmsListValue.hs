-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
  ( Phase1IntegrityAlgorithmsListValue (..),

    -- * Smart constructor
    mkPhase1IntegrityAlgorithmsListValue,

    -- * Lenses
    pialvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The integrity algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1IntegrityAlgorithmsListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsListValue = Phase1IntegrityAlgorithmsListValue'
  { value ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase1IntegrityAlgorithmsListValue' with the minimum fields required to make a request.
--
-- * 'value' - The value for the integrity algorithm.
mkPhase1IntegrityAlgorithmsListValue ::
  Phase1IntegrityAlgorithmsListValue
mkPhase1IntegrityAlgorithmsListValue =
  Phase1IntegrityAlgorithmsListValue' {value = Lude.Nothing}

-- | The value for the integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pialvValue :: Lens.Lens' Phase1IntegrityAlgorithmsListValue (Lude.Maybe Lude.Text)
pialvValue = Lens.lens (value :: Phase1IntegrityAlgorithmsListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase1IntegrityAlgorithmsListValue)
{-# DEPRECATED pialvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Phase1IntegrityAlgorithmsListValue where
  parseXML x =
    Phase1IntegrityAlgorithmsListValue' Lude.<$> (x Lude..@? "value")
