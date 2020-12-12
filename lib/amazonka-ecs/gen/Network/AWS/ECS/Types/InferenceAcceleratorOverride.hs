{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAcceleratorOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAcceleratorOverride
  ( InferenceAcceleratorOverride (..),

    -- * Smart constructor
    mkInferenceAcceleratorOverride,

    -- * Lenses
    iaoDeviceName,
    iaoDeviceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on an Elastic Inference accelerator task override. This parameter is used to override the Elastic Inference accelerator specified in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkInferenceAcceleratorOverride' smart constructor.
data InferenceAcceleratorOverride = InferenceAcceleratorOverride'
  { deviceName ::
      Lude.Maybe Lude.Text,
    deviceType ::
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

-- | Creates a value of 'InferenceAcceleratorOverride' with the minimum fields required to make a request.
--
-- * 'deviceName' - The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
-- * 'deviceType' - The Elastic Inference accelerator type to use.
mkInferenceAcceleratorOverride ::
  InferenceAcceleratorOverride
mkInferenceAcceleratorOverride =
  InferenceAcceleratorOverride'
    { deviceName = Lude.Nothing,
      deviceType = Lude.Nothing
    }

-- | The Elastic Inference accelerator device name to override for the task. This parameter must match a @deviceName@ specified in the task definition.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDeviceName :: Lens.Lens' InferenceAcceleratorOverride (Lude.Maybe Lude.Text)
iaoDeviceName = Lens.lens (deviceName :: InferenceAcceleratorOverride -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: InferenceAcceleratorOverride)
{-# DEPRECATED iaoDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The Elastic Inference accelerator type to use.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDeviceType :: Lens.Lens' InferenceAcceleratorOverride (Lude.Maybe Lude.Text)
iaoDeviceType = Lens.lens (deviceType :: InferenceAcceleratorOverride -> Lude.Maybe Lude.Text) (\s a -> s {deviceType = a} :: InferenceAcceleratorOverride)
{-# DEPRECATED iaoDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

instance Lude.FromJSON InferenceAcceleratorOverride where
  parseJSON =
    Lude.withObject
      "InferenceAcceleratorOverride"
      ( \x ->
          InferenceAcceleratorOverride'
            Lude.<$> (x Lude..:? "deviceName") Lude.<*> (x Lude..:? "deviceType")
      )

instance Lude.ToJSON InferenceAcceleratorOverride where
  toJSON InferenceAcceleratorOverride' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("deviceName" Lude..=) Lude.<$> deviceName,
            ("deviceType" Lude..=) Lude.<$> deviceType
          ]
      )
