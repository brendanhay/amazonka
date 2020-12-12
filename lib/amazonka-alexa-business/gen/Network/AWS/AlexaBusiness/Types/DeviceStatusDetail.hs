{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
  ( DeviceStatusDetail (..),

    -- * Smart constructor
    mkDeviceStatusDetail,

    -- * Lenses
    dsdFeature,
    dsdCode,
  )
where

import Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
import Network.AWS.AlexaBusiness.Types.Feature
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of a device’s status.
--
-- /See:/ 'mkDeviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { feature ::
      Lude.Maybe Feature,
    code :: Lude.Maybe DeviceStatusDetailCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceStatusDetail' with the minimum fields required to make a request.
--
-- * 'code' - The device status detail code.
-- * 'feature' - The list of available features on the device.
mkDeviceStatusDetail ::
  DeviceStatusDetail
mkDeviceStatusDetail =
  DeviceStatusDetail' {feature = Lude.Nothing, code = Lude.Nothing}

-- | The list of available features on the device.
--
-- /Note:/ Consider using 'feature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdFeature :: Lens.Lens' DeviceStatusDetail (Lude.Maybe Feature)
dsdFeature = Lens.lens (feature :: DeviceStatusDetail -> Lude.Maybe Feature) (\s a -> s {feature = a} :: DeviceStatusDetail)
{-# DEPRECATED dsdFeature "Use generic-lens or generic-optics with 'feature' instead." #-}

-- | The device status detail code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdCode :: Lens.Lens' DeviceStatusDetail (Lude.Maybe DeviceStatusDetailCode)
dsdCode = Lens.lens (code :: DeviceStatusDetail -> Lude.Maybe DeviceStatusDetailCode) (\s a -> s {code = a} :: DeviceStatusDetail)
{-# DEPRECATED dsdCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON DeviceStatusDetail where
  parseJSON =
    Lude.withObject
      "DeviceStatusDetail"
      ( \x ->
          DeviceStatusDetail'
            Lude.<$> (x Lude..:? "Feature") Lude.<*> (x Lude..:? "Code")
      )
