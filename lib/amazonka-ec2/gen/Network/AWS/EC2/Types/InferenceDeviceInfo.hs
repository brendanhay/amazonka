{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InferenceDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InferenceDeviceInfo
  ( InferenceDeviceInfo (..),

    -- * Smart constructor
    mkInferenceDeviceInfo,

    -- * Lenses
    idiManufacturer,
    idiCount,
    idiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Inference accelerators for the instance type.
--
-- /See:/ 'mkInferenceDeviceInfo' smart constructor.
data InferenceDeviceInfo = InferenceDeviceInfo'
  { -- | The manufacturer of the Inference accelerator.
    manufacturer :: Lude.Maybe Lude.Text,
    -- | The number of Inference accelerators for the instance type.
    count :: Lude.Maybe Lude.Int,
    -- | The name of the Inference accelerator.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InferenceDeviceInfo' with the minimum fields required to make a request.
--
-- * 'manufacturer' - The manufacturer of the Inference accelerator.
-- * 'count' - The number of Inference accelerators for the instance type.
-- * 'name' - The name of the Inference accelerator.
mkInferenceDeviceInfo ::
  InferenceDeviceInfo
mkInferenceDeviceInfo =
  InferenceDeviceInfo'
    { manufacturer = Lude.Nothing,
      count = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The manufacturer of the Inference accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiManufacturer :: Lens.Lens' InferenceDeviceInfo (Lude.Maybe Lude.Text)
idiManufacturer = Lens.lens (manufacturer :: InferenceDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {manufacturer = a} :: InferenceDeviceInfo)
{-# DEPRECATED idiManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

-- | The number of Inference accelerators for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiCount :: Lens.Lens' InferenceDeviceInfo (Lude.Maybe Lude.Int)
idiCount = Lens.lens (count :: InferenceDeviceInfo -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: InferenceDeviceInfo)
{-# DEPRECATED idiCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The name of the Inference accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idiName :: Lens.Lens' InferenceDeviceInfo (Lude.Maybe Lude.Text)
idiName = Lens.lens (name :: InferenceDeviceInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InferenceDeviceInfo)
{-# DEPRECATED idiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML InferenceDeviceInfo where
  parseXML x =
    InferenceDeviceInfo'
      Lude.<$> (x Lude..@? "manufacturer")
      Lude.<*> (x Lude..@? "count")
      Lude.<*> (x Lude..@? "name")
