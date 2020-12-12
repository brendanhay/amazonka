{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
  ( Phase1IntegrityAlgorithmsRequestListValue (..),

    -- * Smart constructor
    mkPhase1IntegrityAlgorithmsRequestListValue,

    -- * Lenses
    piarlviValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsRequestListValue = Phase1IntegrityAlgorithmsRequestListValue'
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

-- | Creates a value of 'Phase1IntegrityAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The value for the integrity algorithm.
mkPhase1IntegrityAlgorithmsRequestListValue ::
  Phase1IntegrityAlgorithmsRequestListValue
mkPhase1IntegrityAlgorithmsRequestListValue =
  Phase1IntegrityAlgorithmsRequestListValue' {value = Lude.Nothing}

-- | The value for the integrity algorithm.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piarlviValue :: Lens.Lens' Phase1IntegrityAlgorithmsRequestListValue (Lude.Maybe Lude.Text)
piarlviValue = Lens.lens (value :: Phase1IntegrityAlgorithmsRequestListValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Phase1IntegrityAlgorithmsRequestListValue)
{-# DEPRECATED piarlviValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase1IntegrityAlgorithmsRequestListValue where
  toQuery Phase1IntegrityAlgorithmsRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
