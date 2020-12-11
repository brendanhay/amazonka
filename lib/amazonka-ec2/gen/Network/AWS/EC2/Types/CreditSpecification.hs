-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreditSpecification
  ( CreditSpecification (..),

    -- * Smart constructor
    mkCreditSpecification,

    -- * Lenses
    csCPUCredits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the credit option for CPU usage of a T2, T3, or T3a instance.
--
-- /See:/ 'mkCreditSpecification' smart constructor.
newtype CreditSpecification = CreditSpecification'
  { cpuCredits ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreditSpecification' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
mkCreditSpecification ::
  CreditSpecification
mkCreditSpecification =
  CreditSpecification' {cpuCredits = Lude.Nothing}

-- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCPUCredits :: Lens.Lens' CreditSpecification (Lude.Maybe Lude.Text)
csCPUCredits = Lens.lens (cpuCredits :: CreditSpecification -> Lude.Maybe Lude.Text) (\s a -> s {cpuCredits = a} :: CreditSpecification)
{-# DEPRECATED csCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.FromXML CreditSpecification where
  parseXML x =
    CreditSpecification' Lude.<$> (x Lude..@? "cpuCredits")
