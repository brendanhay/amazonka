{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
  ( Phase1DHGroupNumbersRequestListValue (..),

    -- * Smart constructor
    mkPhase1DHGroupNumbersRequestListValue,

    -- * Lenses
    pdhgnrlvValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 1 IKE negotiations.
--
-- /See:/ 'mkPhase1DHGroupNumbersRequestListValue' smart constructor.
newtype Phase1DHGroupNumbersRequestListValue = Phase1DHGroupNumbersRequestListValue'
  { value ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase1DHGroupNumbersRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The Diffie-Hellmann group number.
mkPhase1DHGroupNumbersRequestListValue ::
  Phase1DHGroupNumbersRequestListValue
mkPhase1DHGroupNumbersRequestListValue =
  Phase1DHGroupNumbersRequestListValue' {value = Lude.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnrlvValue :: Lens.Lens' Phase1DHGroupNumbersRequestListValue (Lude.Maybe Lude.Int)
pdhgnrlvValue = Lens.lens (value :: Phase1DHGroupNumbersRequestListValue -> Lude.Maybe Lude.Int) (\s a -> s {value = a} :: Phase1DHGroupNumbersRequestListValue)
{-# DEPRECATED pdhgnrlvValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase1DHGroupNumbersRequestListValue where
  toQuery Phase1DHGroupNumbersRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
