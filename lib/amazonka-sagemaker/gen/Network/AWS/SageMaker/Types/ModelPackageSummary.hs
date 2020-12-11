-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageSummary
  ( ModelPackageSummary (..),

    -- * Smart constructor
    mkModelPackageSummary,

    -- * Lenses
    mpsModelPackageDescription,
    mpsModelPackageName,
    mpsModelPackageARN,
    mpsCreationTime,
    mpsModelPackageStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.ModelPackageStatus

-- | Provides summary information about a model package.
--
-- /See:/ 'mkModelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { modelPackageDescription ::
      Lude.Maybe Lude.Text,
    modelPackageName :: Lude.Text,
    modelPackageARN :: Lude.Text,
    creationTime :: Lude.Timestamp,
    modelPackageStatus :: ModelPackageStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that shows when the model package was created.
-- * 'modelPackageARN' - The Amazon Resource Name (ARN) of the model package.
-- * 'modelPackageDescription' - A brief description of the model package.
-- * 'modelPackageName' - The name of the model package.
-- * 'modelPackageStatus' - The overall status of the model package.
mkModelPackageSummary ::
  -- | 'modelPackageName'
  Lude.Text ->
  -- | 'modelPackageARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'modelPackageStatus'
  ModelPackageStatus ->
  ModelPackageSummary
mkModelPackageSummary
  pModelPackageName_
  pModelPackageARN_
  pCreationTime_
  pModelPackageStatus_ =
    ModelPackageSummary'
      { modelPackageDescription = Lude.Nothing,
        modelPackageName = pModelPackageName_,
        modelPackageARN = pModelPackageARN_,
        creationTime = pCreationTime_,
        modelPackageStatus = pModelPackageStatus_
      }

-- | A brief description of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageDescription :: Lens.Lens' ModelPackageSummary (Lude.Maybe Lude.Text)
mpsModelPackageDescription = Lens.lens (modelPackageDescription :: ModelPackageSummary -> Lude.Maybe Lude.Text) (\s a -> s {modelPackageDescription = a} :: ModelPackageSummary)
{-# DEPRECATED mpsModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

-- | The name of the model package.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageName :: Lens.Lens' ModelPackageSummary Lude.Text
mpsModelPackageName = Lens.lens (modelPackageName :: ModelPackageSummary -> Lude.Text) (\s a -> s {modelPackageName = a} :: ModelPackageSummary)
{-# DEPRECATED mpsModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageARN :: Lens.Lens' ModelPackageSummary Lude.Text
mpsModelPackageARN = Lens.lens (modelPackageARN :: ModelPackageSummary -> Lude.Text) (\s a -> s {modelPackageARN = a} :: ModelPackageSummary)
{-# DEPRECATED mpsModelPackageARN "Use generic-lens or generic-optics with 'modelPackageARN' instead." #-}

-- | A timestamp that shows when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsCreationTime :: Lens.Lens' ModelPackageSummary Lude.Timestamp
mpsCreationTime = Lens.lens (creationTime :: ModelPackageSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ModelPackageSummary)
{-# DEPRECATED mpsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The overall status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageStatus :: Lens.Lens' ModelPackageSummary ModelPackageStatus
mpsModelPackageStatus = Lens.lens (modelPackageStatus :: ModelPackageSummary -> ModelPackageStatus) (\s a -> s {modelPackageStatus = a} :: ModelPackageSummary)
{-# DEPRECATED mpsModelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead." #-}

instance Lude.FromJSON ModelPackageSummary where
  parseJSON =
    Lude.withObject
      "ModelPackageSummary"
      ( \x ->
          ModelPackageSummary'
            Lude.<$> (x Lude..:? "ModelPackageDescription")
            Lude.<*> (x Lude..: "ModelPackageName")
            Lude.<*> (x Lude..: "ModelPackageArn")
            Lude.<*> (x Lude..: "CreationTime")
            Lude.<*> (x Lude..: "ModelPackageStatus")
      )
