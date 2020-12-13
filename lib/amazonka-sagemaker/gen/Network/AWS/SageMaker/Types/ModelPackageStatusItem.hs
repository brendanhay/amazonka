{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusItem
  ( ModelPackageStatusItem (..),

    -- * Smart constructor
    mkModelPackageStatusItem,

    -- * Lenses
    mpsiStatus,
    mpsiFailureReason,
    mpsiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus

-- | Represents the overall status of a model package.
--
-- /See:/ 'mkModelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { -- | The current status.
    status :: DetailedModelPackageStatus,
    -- | if the overall status is @Failed@ , the reason for the failure.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The name of the model package for which the overall status is being reported.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageStatusItem' with the minimum fields required to make a request.
--
-- * 'status' - The current status.
-- * 'failureReason' - if the overall status is @Failed@ , the reason for the failure.
-- * 'name' - The name of the model package for which the overall status is being reported.
mkModelPackageStatusItem ::
  -- | 'status'
  DetailedModelPackageStatus ->
  -- | 'name'
  Lude.Text ->
  ModelPackageStatusItem
mkModelPackageStatusItem pStatus_ pName_ =
  ModelPackageStatusItem'
    { status = pStatus_,
      failureReason = Lude.Nothing,
      name = pName_
    }

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiStatus :: Lens.Lens' ModelPackageStatusItem DetailedModelPackageStatus
mpsiStatus = Lens.lens (status :: ModelPackageStatusItem -> DetailedModelPackageStatus) (\s a -> s {status = a} :: ModelPackageStatusItem)
{-# DEPRECATED mpsiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | if the overall status is @Failed@ , the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiFailureReason :: Lens.Lens' ModelPackageStatusItem (Lude.Maybe Lude.Text)
mpsiFailureReason = Lens.lens (failureReason :: ModelPackageStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: ModelPackageStatusItem)
{-# DEPRECATED mpsiFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the model package for which the overall status is being reported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiName :: Lens.Lens' ModelPackageStatusItem Lude.Text
mpsiName = Lens.lens (name :: ModelPackageStatusItem -> Lude.Text) (\s a -> s {name = a} :: ModelPackageStatusItem)
{-# DEPRECATED mpsiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ModelPackageStatusItem where
  parseJSON =
    Lude.withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "Name")
      )
