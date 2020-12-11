-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceFamilyCreditSpecification
  ( InstanceFamilyCreditSpecification (..),

    -- * Smart constructor
    mkInstanceFamilyCreditSpecification,

    -- * Lenses
    ifcsInstanceFamily,
    ifcsCPUCredits,
  )
where

import Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the default credit option for CPU usage of a burstable performance instance family.
--
-- /See:/ 'mkInstanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { instanceFamily ::
      Lude.Maybe
        UnlimitedSupportedInstanceFamily,
    cpuCredits ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFamilyCreditSpecification' with the minimum fields required to make a request.
--
-- * 'cpuCredits' - The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
-- * 'instanceFamily' - The instance family.
mkInstanceFamilyCreditSpecification ::
  InstanceFamilyCreditSpecification
mkInstanceFamilyCreditSpecification =
  InstanceFamilyCreditSpecification'
    { instanceFamily = Lude.Nothing,
      cpuCredits = Lude.Nothing
    }

-- | The instance family.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsInstanceFamily :: Lens.Lens' InstanceFamilyCreditSpecification (Lude.Maybe UnlimitedSupportedInstanceFamily)
ifcsInstanceFamily = Lens.lens (instanceFamily :: InstanceFamilyCreditSpecification -> Lude.Maybe UnlimitedSupportedInstanceFamily) (\s a -> s {instanceFamily = a} :: InstanceFamilyCreditSpecification)
{-# DEPRECATED ifcsInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The default credit option for CPU usage of the instance family. Valid values are @standard@ and @unlimited@ .
--
-- /Note:/ Consider using 'cpuCredits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifcsCPUCredits :: Lens.Lens' InstanceFamilyCreditSpecification (Lude.Maybe Lude.Text)
ifcsCPUCredits = Lens.lens (cpuCredits :: InstanceFamilyCreditSpecification -> Lude.Maybe Lude.Text) (\s a -> s {cpuCredits = a} :: InstanceFamilyCreditSpecification)
{-# DEPRECATED ifcsCPUCredits "Use generic-lens or generic-optics with 'cpuCredits' instead." #-}

instance Lude.FromXML InstanceFamilyCreditSpecification where
  parseXML x =
    InstanceFamilyCreditSpecification'
      Lude.<$> (x Lude..@? "instanceFamily") Lude.<*> (x Lude..@? "cpuCredits")
