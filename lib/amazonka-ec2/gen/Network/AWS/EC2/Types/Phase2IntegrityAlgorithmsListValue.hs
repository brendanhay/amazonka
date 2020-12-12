{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsListValue
  ( Phase2IntegrityAlgorithmsListValue (..),

    -- * Smart constructor
    mkPhase2IntegrityAlgorithmsListValue,

    -- * Lenses
    phaValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The integrity algorithm for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2IntegrityAlgorithmsListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsListValue = Phase2IntegrityAlgorithmsListValue'
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

-- | Creates a value of 'Phase2IntegrityAlgorithmsListValue' with the minimum fields required to make a request.
--
-- * 'value' - The integrity algorithm.
mkPhase2IntegrityAlgorithmsListValue ::
  Phase2IntegrityAlgorithmsListValue
mkPhase2IntegrityAlgorithmsListValue =
  Phase2IntegrityAlgorithmsListValue' {value = Lude.Nothing}

-- | The integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
phaValue :: Lens.Lens' Phase2IntegrityAlgorithmsListValue (Lude.Maybe Lude.Text)
phaValue = Lens.lens (value :: Phase2IntegrityAlgorithmsListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase2IntegrityAlgorithmsListValue)
{-# DEPRECATED phaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Phase2IntegrityAlgorithmsListValue where
  parseXML x =
    Phase2IntegrityAlgorithmsListValue' Lude.<$> (x Lude..@? "value")
