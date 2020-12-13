{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceSpec
  ( ResourceSpec (..),

    -- * Smart constructor
    mkResourceSpec,

    -- * Lenses
    rsInstanceType,
    rsSageMakerImageARN,
    rsSageMakerImageVersionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AppInstanceType

-- | Specifies the ARN's of a SageMaker image and SageMaker image version, and the instance type that the version runs on.
--
-- /See:/ 'mkResourceSpec' smart constructor.
data ResourceSpec = ResourceSpec'
  { -- | The instance type that the image version runs on.
    instanceType :: Lude.Maybe AppInstanceType,
    -- | The ARN of the SageMaker image that the image version belongs to.
    sageMakerImageARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the image version created on the instance.
    sageMakerImageVersionARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceSpec' with the minimum fields required to make a request.
--
-- * 'instanceType' - The instance type that the image version runs on.
-- * 'sageMakerImageARN' - The ARN of the SageMaker image that the image version belongs to.
-- * 'sageMakerImageVersionARN' - The ARN of the image version created on the instance.
mkResourceSpec ::
  ResourceSpec
mkResourceSpec =
  ResourceSpec'
    { instanceType = Lude.Nothing,
      sageMakerImageARN = Lude.Nothing,
      sageMakerImageVersionARN = Lude.Nothing
    }

-- | The instance type that the image version runs on.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsInstanceType :: Lens.Lens' ResourceSpec (Lude.Maybe AppInstanceType)
rsInstanceType = Lens.lens (instanceType :: ResourceSpec -> Lude.Maybe AppInstanceType) (\s a -> s {instanceType = a} :: ResourceSpec)
{-# DEPRECATED rsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ARN of the SageMaker image that the image version belongs to.
--
-- /Note:/ Consider using 'sageMakerImageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSageMakerImageARN :: Lens.Lens' ResourceSpec (Lude.Maybe Lude.Text)
rsSageMakerImageARN = Lens.lens (sageMakerImageARN :: ResourceSpec -> Lude.Maybe Lude.Text) (\s a -> s {sageMakerImageARN = a} :: ResourceSpec)
{-# DEPRECATED rsSageMakerImageARN "Use generic-lens or generic-optics with 'sageMakerImageARN' instead." #-}

-- | The ARN of the image version created on the instance.
--
-- /Note:/ Consider using 'sageMakerImageVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSageMakerImageVersionARN :: Lens.Lens' ResourceSpec (Lude.Maybe Lude.Text)
rsSageMakerImageVersionARN = Lens.lens (sageMakerImageVersionARN :: ResourceSpec -> Lude.Maybe Lude.Text) (\s a -> s {sageMakerImageVersionARN = a} :: ResourceSpec)
{-# DEPRECATED rsSageMakerImageVersionARN "Use generic-lens or generic-optics with 'sageMakerImageVersionARN' instead." #-}

instance Lude.FromJSON ResourceSpec where
  parseJSON =
    Lude.withObject
      "ResourceSpec"
      ( \x ->
          ResourceSpec'
            Lude.<$> (x Lude..:? "InstanceType")
            Lude.<*> (x Lude..:? "SageMakerImageArn")
            Lude.<*> (x Lude..:? "SageMakerImageVersionArn")
      )

instance Lude.ToJSON ResourceSpec where
  toJSON ResourceSpec' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceType" Lude..=) Lude.<$> instanceType,
            ("SageMakerImageArn" Lude..=) Lude.<$> sageMakerImageARN,
            ("SageMakerImageVersionArn" Lude..=)
              Lude.<$> sageMakerImageVersionARN
          ]
      )
