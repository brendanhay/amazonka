{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mpsModelPackageName,
    mpsModelPackageArn,
    mpsCreationTime,
    mpsModelPackageStatus,
    mpsModelPackageDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.EntityDescription as Types
import qualified Network.AWS.SageMaker.Types.EntityName as Types
import qualified Network.AWS.SageMaker.Types.ModelPackageArn as Types
import qualified Network.AWS.SageMaker.Types.ModelPackageStatus as Types

-- | Provides summary information about a model package.
--
-- /See:/ 'mkModelPackageSummary' smart constructor.
data ModelPackageSummary = ModelPackageSummary'
  { -- | The name of the model package.
    modelPackageName :: Types.EntityName,
    -- | The Amazon Resource Name (ARN) of the model package.
    modelPackageArn :: Types.ModelPackageArn,
    -- | A timestamp that shows when the model package was created.
    creationTime :: Core.NominalDiffTime,
    -- | The overall status of the model package.
    modelPackageStatus :: Types.ModelPackageStatus,
    -- | A brief description of the model package.
    modelPackageDescription :: Core.Maybe Types.EntityDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModelPackageSummary' value with any optional fields omitted.
mkModelPackageSummary ::
  -- | 'modelPackageName'
  Types.EntityName ->
  -- | 'modelPackageArn'
  Types.ModelPackageArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'modelPackageStatus'
  Types.ModelPackageStatus ->
  ModelPackageSummary
mkModelPackageSummary
  modelPackageName
  modelPackageArn
  creationTime
  modelPackageStatus =
    ModelPackageSummary'
      { modelPackageName,
        modelPackageArn,
        creationTime,
        modelPackageStatus,
        modelPackageDescription = Core.Nothing
      }

-- | The name of the model package.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageName :: Lens.Lens' ModelPackageSummary Types.EntityName
mpsModelPackageName = Lens.field @"modelPackageName"
{-# DEPRECATED mpsModelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageArn :: Lens.Lens' ModelPackageSummary Types.ModelPackageArn
mpsModelPackageArn = Lens.field @"modelPackageArn"
{-# DEPRECATED mpsModelPackageArn "Use generic-lens or generic-optics with 'modelPackageArn' instead." #-}

-- | A timestamp that shows when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsCreationTime :: Lens.Lens' ModelPackageSummary Core.NominalDiffTime
mpsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED mpsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The overall status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageStatus :: Lens.Lens' ModelPackageSummary Types.ModelPackageStatus
mpsModelPackageStatus = Lens.field @"modelPackageStatus"
{-# DEPRECATED mpsModelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead." #-}

-- | A brief description of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageDescription :: Lens.Lens' ModelPackageSummary (Core.Maybe Types.EntityDescription)
mpsModelPackageDescription = Lens.field @"modelPackageDescription"
{-# DEPRECATED mpsModelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead." #-}

instance Core.FromJSON ModelPackageSummary where
  parseJSON =
    Core.withObject "ModelPackageSummary" Core.$
      \x ->
        ModelPackageSummary'
          Core.<$> (x Core..: "ModelPackageName")
          Core.<*> (x Core..: "ModelPackageArn")
          Core.<*> (x Core..: "CreationTime")
          Core.<*> (x Core..: "ModelPackageStatus")
          Core.<*> (x Core..:? "ModelPackageDescription")
