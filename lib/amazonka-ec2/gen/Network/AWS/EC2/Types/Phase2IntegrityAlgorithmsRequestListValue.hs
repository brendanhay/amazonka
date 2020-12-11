-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
  ( Phase2IntegrityAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase2IntegrityAlgorithmsRequestListValue,

    -- * Lenses
    piarlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase2IntegrityAlgorithmsRequestListValue = Phase2IntegrityAlgorithmsRequestListValue'
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

-- | Creates a value of 'Phase2IntegrityAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The integrity algorithm.
mkPhase2IntegrityAlgorithmsRequestListValue ::
  Phase2IntegrityAlgorithmsRequestListValue
mkPhase2IntegrityAlgorithmsRequestListValue =
  Phase2IntegrityAlgorithmsRequestListValue' {value = Lude.Nothing}

-- | The integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piarlvValue :: Lens.Lens' Phase2IntegrityAlgorithmsRequestListValue (Lude.Maybe Lude.Text)
piarlvValue = Lens.lens (value :: Phase2IntegrityAlgorithmsRequestListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase2IntegrityAlgorithmsRequestListValue)
{-# DEPRECATED piarlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase2IntegrityAlgorithmsRequestListValue where
  toQuery Phase2IntegrityAlgorithmsRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
