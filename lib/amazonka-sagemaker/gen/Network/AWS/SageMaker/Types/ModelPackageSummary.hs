{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageSummary
  ( ModelPackageSummary (..)
  -- * Smart constructor
  , mkModelPackageSummary
  -- * Lenses
  , mpsModelPackageName
  , mpsModelPackageArn
  , mpsCreationTime
  , mpsModelPackageStatus
  , mpsModelPackageDescription
  ) where

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
  { modelPackageName :: Types.EntityName
    -- ^ The name of the model package.
  , modelPackageArn :: Types.ModelPackageArn
    -- ^ The Amazon Resource Name (ARN) of the model package.
  , creationTime :: Core.NominalDiffTime
    -- ^ A timestamp that shows when the model package was created.
  , modelPackageStatus :: Types.ModelPackageStatus
    -- ^ The overall status of the model package.
  , modelPackageDescription :: Core.Maybe Types.EntityDescription
    -- ^ A brief description of the model package.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModelPackageSummary' value with any optional fields omitted.
mkModelPackageSummary
    :: Types.EntityName -- ^ 'modelPackageName'
    -> Types.ModelPackageArn -- ^ 'modelPackageArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Types.ModelPackageStatus -- ^ 'modelPackageStatus'
    -> ModelPackageSummary
mkModelPackageSummary modelPackageName modelPackageArn creationTime
  modelPackageStatus
  = ModelPackageSummary'{modelPackageName, modelPackageArn,
                         creationTime, modelPackageStatus,
                         modelPackageDescription = Core.Nothing}

-- | The name of the model package.
--
-- /Note:/ Consider using 'modelPackageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageName :: Lens.Lens' ModelPackageSummary Types.EntityName
mpsModelPackageName = Lens.field @"modelPackageName"
{-# INLINEABLE mpsModelPackageName #-}
{-# DEPRECATED modelPackageName "Use generic-lens or generic-optics with 'modelPackageName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the model package.
--
-- /Note:/ Consider using 'modelPackageArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageArn :: Lens.Lens' ModelPackageSummary Types.ModelPackageArn
mpsModelPackageArn = Lens.field @"modelPackageArn"
{-# INLINEABLE mpsModelPackageArn #-}
{-# DEPRECATED modelPackageArn "Use generic-lens or generic-optics with 'modelPackageArn' instead"  #-}

-- | A timestamp that shows when the model package was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsCreationTime :: Lens.Lens' ModelPackageSummary Core.NominalDiffTime
mpsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE mpsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The overall status of the model package.
--
-- /Note:/ Consider using 'modelPackageStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageStatus :: Lens.Lens' ModelPackageSummary Types.ModelPackageStatus
mpsModelPackageStatus = Lens.field @"modelPackageStatus"
{-# INLINEABLE mpsModelPackageStatus #-}
{-# DEPRECATED modelPackageStatus "Use generic-lens or generic-optics with 'modelPackageStatus' instead"  #-}

-- | A brief description of the model package.
--
-- /Note:/ Consider using 'modelPackageDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsModelPackageDescription :: Lens.Lens' ModelPackageSummary (Core.Maybe Types.EntityDescription)
mpsModelPackageDescription = Lens.field @"modelPackageDescription"
{-# INLINEABLE mpsModelPackageDescription #-}
{-# DEPRECATED modelPackageDescription "Use generic-lens or generic-optics with 'modelPackageDescription' instead"  #-}

instance Core.FromJSON ModelPackageSummary where
        parseJSON
          = Core.withObject "ModelPackageSummary" Core.$
              \ x ->
                ModelPackageSummary' Core.<$>
                  (x Core..: "ModelPackageName") Core.<*> x Core..: "ModelPackageArn"
                    Core.<*> x Core..: "CreationTime"
                    Core.<*> x Core..: "ModelPackageStatus"
                    Core.<*> x Core..:? "ModelPackageDescription"
