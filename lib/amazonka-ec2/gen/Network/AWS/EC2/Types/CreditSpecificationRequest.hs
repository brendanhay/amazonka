{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreditSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CreditSpecificationRequest
  ( CreditSpecificationRequest (..)
  -- * Smart constructor
  , mkCreditSpecificationRequest
  -- * Lenses
  , csrCpuCredits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The credit option for CPU usage of a T2, T3, or T3a instance.
--
-- /See:/ 'mkCreditSpecificationRequest' smart constructor.
newtype CreditSpecificationRequest = CreditSpecificationRequest'
  { cpuCredits :: Core.Text
    -- ^ The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreditSpecificationRequest' value with any optional fields omitted.
mkCreditSpecificationRequest
    :: Core.Text -- ^ 'cpuCredits'
    -> CreditSpecificationRequest
mkCreditSpecificationRequest cpuCredits
  = CreditSpecificationRequest'{cpuCredits}

-- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrCpuCredits :: Lens.Lens' CreditSpecificationRequest Core.Text
csrCpuCredits = Lens.field @"cpuCredits"
{-# INLINEABLE csrCpuCredits #-}
{-# DEPRECATED cpuCredits "Use generic-lens or generic-optics with 'cpuCredits' instead"  #-}

instance Core.ToQuery CreditSpecificationRequest where
        toQuery CreditSpecificationRequest{..}
          = Core.toQueryPair "CpuCredits" cpuCredits
