{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
  ( SageMakerMachineLearningModelResourceData (..),

    -- * Smart constructor
    mkSageMakerMachineLearningModelResourceData,

    -- * Lenses
    smmlmrdOwnerSetting,
    smmlmrdSageMakerJobARN,
    smmlmrdDestinationPath,
  )
where

import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /See:/ 'mkSageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { ownerSetting :: Lude.Maybe ResourceDownloadOwnerSetting,
    -- | The ARN of the Amazon SageMaker training job that represents the source model.
    sageMakerJobARN :: Lude.Maybe Lude.Text,
    -- | The absolute local path of the resource inside the Lambda environment.
    destinationPath :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SageMakerMachineLearningModelResourceData' with the minimum fields required to make a request.
--
-- * 'ownerSetting' -
-- * 'sageMakerJobARN' - The ARN of the Amazon SageMaker training job that represents the source model.
-- * 'destinationPath' - The absolute local path of the resource inside the Lambda environment.
mkSageMakerMachineLearningModelResourceData ::
  SageMakerMachineLearningModelResourceData
mkSageMakerMachineLearningModelResourceData =
  SageMakerMachineLearningModelResourceData'
    { ownerSetting =
        Lude.Nothing,
      sageMakerJobARN = Lude.Nothing,
      destinationPath = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdOwnerSetting :: Lens.Lens' SageMakerMachineLearningModelResourceData (Lude.Maybe ResourceDownloadOwnerSetting)
smmlmrdOwnerSetting = Lens.lens (ownerSetting :: SageMakerMachineLearningModelResourceData -> Lude.Maybe ResourceDownloadOwnerSetting) (\s a -> s {ownerSetting = a} :: SageMakerMachineLearningModelResourceData)
{-# DEPRECATED smmlmrdOwnerSetting "Use generic-lens or generic-optics with 'ownerSetting' instead." #-}

-- | The ARN of the Amazon SageMaker training job that represents the source model.
--
-- /Note:/ Consider using 'sageMakerJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdSageMakerJobARN :: Lens.Lens' SageMakerMachineLearningModelResourceData (Lude.Maybe Lude.Text)
smmlmrdSageMakerJobARN = Lens.lens (sageMakerJobARN :: SageMakerMachineLearningModelResourceData -> Lude.Maybe Lude.Text) (\s a -> s {sageMakerJobARN = a} :: SageMakerMachineLearningModelResourceData)
{-# DEPRECATED smmlmrdSageMakerJobARN "Use generic-lens or generic-optics with 'sageMakerJobARN' instead." #-}

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdDestinationPath :: Lens.Lens' SageMakerMachineLearningModelResourceData (Lude.Maybe Lude.Text)
smmlmrdDestinationPath = Lens.lens (destinationPath :: SageMakerMachineLearningModelResourceData -> Lude.Maybe Lude.Text) (\s a -> s {destinationPath = a} :: SageMakerMachineLearningModelResourceData)
{-# DEPRECATED smmlmrdDestinationPath "Use generic-lens or generic-optics with 'destinationPath' instead." #-}

instance Lude.FromJSON SageMakerMachineLearningModelResourceData where
  parseJSON =
    Lude.withObject
      "SageMakerMachineLearningModelResourceData"
      ( \x ->
          SageMakerMachineLearningModelResourceData'
            Lude.<$> (x Lude..:? "OwnerSetting")
            Lude.<*> (x Lude..:? "SageMakerJobArn")
            Lude.<*> (x Lude..:? "DestinationPath")
      )

instance Lude.ToJSON SageMakerMachineLearningModelResourceData where
  toJSON SageMakerMachineLearningModelResourceData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OwnerSetting" Lude..=) Lude.<$> ownerSetting,
            ("SageMakerJobArn" Lude..=) Lude.<$> sageMakerJobARN,
            ("DestinationPath" Lude..=) Lude.<$> destinationPath
          ]
      )
