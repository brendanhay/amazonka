{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLContainerDefinition
  ( AutoMLContainerDefinition (..),

    -- * Smart constructor
    mkAutoMLContainerDefinition,

    -- * Lenses
    amlcdModelDataURL,
    amlcdImage,
    amlcdEnvironment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of container definitions that describe the different containers that make up one AutoML candidate. Refer to ContainerDefinition for more details.
--
-- /See:/ 'mkAutoMLContainerDefinition' smart constructor.
data AutoMLContainerDefinition = AutoMLContainerDefinition'
  { -- | The location of the model artifacts. Refer to ContainerDefinition for more details.
    modelDataURL :: Lude.Text,
    -- | The ECR path of the container. Refer to ContainerDefinition for more details.
    image :: Lude.Text,
    -- | Environment variables to set in the container. Refer to ContainerDefinition for more details.
    environment :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLContainerDefinition' with the minimum fields required to make a request.
--
-- * 'modelDataURL' - The location of the model artifacts. Refer to ContainerDefinition for more details.
-- * 'image' - The ECR path of the container. Refer to ContainerDefinition for more details.
-- * 'environment' - Environment variables to set in the container. Refer to ContainerDefinition for more details.
mkAutoMLContainerDefinition ::
  -- | 'modelDataURL'
  Lude.Text ->
  -- | 'image'
  Lude.Text ->
  AutoMLContainerDefinition
mkAutoMLContainerDefinition pModelDataURL_ pImage_ =
  AutoMLContainerDefinition'
    { modelDataURL = pModelDataURL_,
      image = pImage_,
      environment = Lude.Nothing
    }

-- | The location of the model artifacts. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'modelDataURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdModelDataURL :: Lens.Lens' AutoMLContainerDefinition Lude.Text
amlcdModelDataURL = Lens.lens (modelDataURL :: AutoMLContainerDefinition -> Lude.Text) (\s a -> s {modelDataURL = a} :: AutoMLContainerDefinition)
{-# DEPRECATED amlcdModelDataURL "Use generic-lens or generic-optics with 'modelDataURL' instead." #-}

-- | The ECR path of the container. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdImage :: Lens.Lens' AutoMLContainerDefinition Lude.Text
amlcdImage = Lens.lens (image :: AutoMLContainerDefinition -> Lude.Text) (\s a -> s {image = a} :: AutoMLContainerDefinition)
{-# DEPRECATED amlcdImage "Use generic-lens or generic-optics with 'image' instead." #-}

-- | Environment variables to set in the container. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdEnvironment :: Lens.Lens' AutoMLContainerDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
amlcdEnvironment = Lens.lens (environment :: AutoMLContainerDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: AutoMLContainerDefinition)
{-# DEPRECATED amlcdEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

instance Lude.FromJSON AutoMLContainerDefinition where
  parseJSON =
    Lude.withObject
      "AutoMLContainerDefinition"
      ( \x ->
          AutoMLContainerDefinition'
            Lude.<$> (x Lude..: "ModelDataUrl")
            Lude.<*> (x Lude..: "Image")
            Lude.<*> (x Lude..:? "Environment" Lude..!= Lude.mempty)
      )
