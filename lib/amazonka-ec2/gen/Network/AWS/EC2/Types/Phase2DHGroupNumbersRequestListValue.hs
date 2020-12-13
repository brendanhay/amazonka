{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
  ( Phase2DHGroupNumbersRequestListValue (..),

    -- * Smart constructor
    mkPhase2DHGroupNumbersRequestListValue,

    -- * Lenses
    pdhgnrlvfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 2 IKE negotiations.
--
-- /See:/ 'mkPhase2DHGroupNumbersRequestListValue' smart constructor.
newtype Phase2DHGroupNumbersRequestListValue = Phase2DHGroupNumbersRequestListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Phase2DHGroupNumbersRequestListValue' with the minimum fields required to make a request.
--
-- * 'value' - The Diffie-Hellmann group number.
mkPhase2DHGroupNumbersRequestListValue ::
  Phase2DHGroupNumbersRequestListValue
mkPhase2DHGroupNumbersRequestListValue =
  Phase2DHGroupNumbersRequestListValue' {value = Lude.Nothing}

-- | The Diffie-Hellmann group number.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdhgnrlvfValue :: Lens.Lens' Phase2DHGroupNumbersRequestListValue (Lude.Maybe Lude.Int)
pdhgnrlvfValue = Lens.lens (value :: Phase2DHGroupNumbersRequestListValue -> Lude.Maybe Lude.Int) (\s a -> s {value = a} :: Phase2DHGroupNumbersRequestListValue)
{-# DEPRECATED pdhgnrlvfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery Phase2DHGroupNumbersRequestListValue where
  toQuery Phase2DHGroupNumbersRequestListValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
