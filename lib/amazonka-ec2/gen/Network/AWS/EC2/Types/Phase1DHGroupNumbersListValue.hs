{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
  ( Phase1DHGroupNumbersListValue (..),

    -- * Smart constructor
    mkPhase1DHGroupNumbersListValue,

    -- * Lenses
    pdhgnlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Diffie-Hellmann group number for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1DHGroupNumbersListValue' smart constructor.
newtype Phase1DHGroupNumbersListValue = Phase1DHGroupNumbersListValue'
  { value ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase1DHGroupNumbersListValue' with the minimum fields required to make a request.
--
-- * 'value' - The Diffie-Hellmann group number.
mkPhase1DHGroupNumbersListValue ::
  Phase1DHGroupNumbersListValue
mkPhase1DHGroupNumbersListValue =
  Phase1DHGroupNumbersListValue' {value = Lude.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnlvValue :: Lens.Lens' Phase1DHGroupNumbersListValue (Lude.Maybe Lude.Int)
pdhgnlvValue = Lens.lens (value :: Phase1DHGroupNumbersListValue -> Lude.Maybe Lude.Int) (\s a -> s {value = a} :: Phase1DHGroupNumbersListValue)
{-# DEPRECATED pdhgnlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromXML Phase1DHGroupNumbersListValue where
  parseXML x =
    Phase1DHGroupNumbersListValue' Lude.<$> (x Lude..@? "value")
