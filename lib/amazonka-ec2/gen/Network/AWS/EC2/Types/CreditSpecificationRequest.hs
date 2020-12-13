{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreditSpecificationRequest
  ( CreditSpecificationRequest (..),

    -- * Smart constructor
    mkCreditSpecificationRequest,

    -- * Lenses
    csrCPUCredits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The credit option for CPU usage of a T2, T3, or T3a instance.
--
-- /See:/ 'mkCreditSpecificationRequest' smart constructor.
newtype CreditSpecificationRequest = CreditSpecificationRequest'
  { -- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
    cpuCredits :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreditSpecificationRequest' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
mkCreditSpecificationRequest ::
  -- | 'cpuCredits'
  Lude.Text ->
  CreditSpecificationRequest
mkCreditSpecificationRequest pCPUCredits_ =
  CreditSpecificationRequest' {cpuCredits = pCPUCredits_}

-- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrCPUCredits :: Lens.Lens' CreditSpecificationRequest Lude.Text
csrCPUCredits = Lens.lens (cpuCredits :: CreditSpecificationRequest -> Lude.Text) (\s a -> s {cpuCredits = a} :: CreditSpecificationRequest)
{-# DEPRECATED csrCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.ToQuery CreditSpecificationRequest where
  toQuery CreditSpecificationRequest' {..} =
    Lude.mconcat ["CpuCredits" Lude.=: cpuCredits]
