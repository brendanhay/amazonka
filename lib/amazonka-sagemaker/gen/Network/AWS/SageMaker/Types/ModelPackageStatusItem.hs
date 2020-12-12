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
    mpsiFailureReason,
    mpsiName,
    mpsiStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.DetailedModelPackageStatus

-- | Represents the overall status of a model package.
--
-- /See:/ 'mkModelPackageStatusItem' smart constructor.
data ModelPackageStatusItem = ModelPackageStatusItem'
  { failureReason ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    status :: DetailedModelPackageStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelPackageStatusItem' with the minimum fields required to make a request.
--
-- * 'failureReason' - if the overall status is @Failed@ , the reason for the failure.
-- * 'name' - The name of the model package for which the overall status is being reported.
-- * 'status' - The current status.
mkModelPackageStatusItem ::
  -- | 'name'
  Lude.Text ->
  -- | 'status'
  DetailedModelPackageStatus ->
  ModelPackageStatusItem
mkModelPackageStatusItem pName_ pStatus_ =
  ModelPackageStatusItem'
    { failureReason = Lude.Nothing,
      name = pName_,
      status = pStatus_
    }

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

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpsiStatus :: Lens.Lens' ModelPackageStatusItem DetailedModelPackageStatus
mpsiStatus = Lens.lens (status :: ModelPackageStatusItem -> DetailedModelPackageStatus) (\s a -> s {status = a} :: ModelPackageStatusItem)
{-# DEPRECATED mpsiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON ModelPackageStatusItem where
  parseJSON =
    Lude.withObject
      "ModelPackageStatusItem"
      ( \x ->
          ModelPackageStatusItem'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Status")
      )
