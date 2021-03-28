{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ModelPackageStatusItem
  ( ModelPackageStatusItem (..)
  -- * Smart constructor
  , mkModelPackageStatusItem
  -- * Lenses
  , mpsiName
  , mpsiStatus
  , mpsiFailureReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DetailedModelPackageStatus as Types
import qualified Network.AWS.SageMaker.Types.Name as Types

-- | Represents the overall status of a model package.
--
-- /See:/ 'mkModelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { name :: Types.Name
    -- ^ The name of the model package for which the overall status is being reported.
  , status :: Types.DetailedModelPackageStatus
    -- ^ The current status.
  , failureReason :: Core.Maybe Core.Text
    -- ^ if the overall status is @Failed@ , the reason for the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModelPackageStatusItem' value with any optional fields omitted.
mkModelPackageStatusItem
    :: Types.Name -- ^ 'name'
    -> Types.DetailedModelPackageStatus -- ^ 'status'
    -> ModelPackageStatusItem
mkModelPackageStatusItem name status
  = ModelPackageStatusItem'{name, status,
                            failureReason = Core.Nothing}

-- | The name of the model package for which the overall status is being reported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiName :: Lens.Lens' ModelPackageStatusItem Types.Name
mpsiName = Lens.field @"name"
{-# INLINEABLE mpsiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiStatus :: Lens.Lens' ModelPackageStatusItem Types.DetailedModelPackageStatus
mpsiStatus = Lens.field @"status"
{-# INLINEABLE mpsiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | if the overall status is @Failed@ , the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiFailureReason :: Lens.Lens' ModelPackageStatusItem (Core.Maybe Core.Text)
mpsiFailureReason = Lens.field @"failureReason"
{-# INLINEABLE mpsiFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

instance Core.FromJSON ModelPackageStatusItem where
        parseJSON
          = Core.withObject "ModelPackageStatusItem" Core.$
              \ x ->
                ModelPackageStatusItem' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Status" Core.<*>
                    x Core..:? "FailureReason"
