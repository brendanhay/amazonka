-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCreditSpecification
  ( InstanceCreditSpecification (..),

    -- * Smart constructor
    mkInstanceCreditSpecification,

    -- * Lenses
    icsInstanceId,
    icsCPUCredits,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the credit option for CPU usage of a burstable performance instance.
--
-- /See:/ 'mkInstanceCreditSpecification' smart constructor.
data InstanceCreditSpecification = InstanceCreditSpecification'
  { instanceId ::
      Lude.Maybe Lude.Text,
    cpuCredits :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCreditSpecification' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
-- * 'instanceId' - The ID of the instance.
mkInstanceCreditSpecification ::
  InstanceCreditSpecification
mkInstanceCreditSpecification =
  InstanceCreditSpecification'
    { instanceId = Lude.Nothing,
      cpuCredits = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsInstanceId :: Lens.Lens' InstanceCreditSpecification (Lude.Maybe Lude.Text)
icsInstanceId = Lens.lens (instanceId :: InstanceCreditSpecification -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceCreditSpecification)
{-# DEPRECATED icsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The credit option for CPU usage of the instance. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icsCPUCredits :: Lens.Lens' InstanceCreditSpecification (Lude.Maybe Lude.Text)
icsCPUCredits = Lens.lens (cpuCredits :: InstanceCreditSpecification -> Lude.Maybe Lude.Text) (\s a -> s {cpuCredits = a} :: InstanceCreditSpecification)
{-# DEPRECATED icsCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.FromXML InstanceCreditSpecification where
  parseXML x =
    InstanceCreditSpecification'
      Lude.<$> (x Lude..@? "instanceId") Lude.<*> (x Lude..@? "cpuCredits")
