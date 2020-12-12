{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.InferenceAccelerator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.InferenceAccelerator
  ( InferenceAccelerator (..),

    -- * Smart constructor
    mkInferenceAccelerator,

    -- * Lenses
    iaDeviceName,
    iaDeviceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on a Elastic Inference accelerator. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-eia.html Working with Amazon Elastic Inference on Amazon ECS> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkInferenceAccelerator' smart constructor.
data InferenceAccelerator = InferenceAccelerator'
  { deviceName ::
      Lude.Text,
    deviceType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InferenceAccelerator' with the minimum fields required to make a request.
--
-- * 'deviceName' - The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
-- * 'deviceType' - The Elastic Inference accelerator type to use.
mkInferenceAccelerator ::
  -- | 'deviceName'
  Lude.Text ->
  -- | 'deviceType'
  Lude.Text ->
  InferenceAccelerator
mkInferenceAccelerator pDeviceName_ pDeviceType_ =
  InferenceAccelerator'
    { deviceName = pDeviceName_,
      deviceType = pDeviceType_
    }

-- | The Elastic Inference accelerator device name. The @deviceName@ must also be referenced in a container definition as a 'ResourceRequirement' .
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaDeviceName :: Lens.Lens' InferenceAccelerator Lude.Text
iaDeviceName = Lens.lens (deviceName :: InferenceAccelerator -> Lude.Text) (\s a -> s {deviceName = a} :: InferenceAccelerator)
{-# DEPRECATED iaDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

-- | The Elastic Inference accelerator type to use.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaDeviceType :: Lens.Lens' InferenceAccelerator Lude.Text
iaDeviceType = Lens.lens (deviceType :: InferenceAccelerator -> Lude.Text) (\s a -> s {deviceType = a} :: InferenceAccelerator)
{-# DEPRECATED iaDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

instance Lude.FromJSON InferenceAccelerator where
  parseJSON =
    Lude.withObject
      "InferenceAccelerator"
      ( \x ->
          InferenceAccelerator'
            Lude.<$> (x Lude..: "deviceName") Lude.<*> (x Lude..: "deviceType")
      )

instance Lude.ToJSON InferenceAccelerator where
  toJSON InferenceAccelerator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("deviceName" Lude..= deviceName),
            Lude.Just ("deviceType" Lude..= deviceType)
          ]
      )
