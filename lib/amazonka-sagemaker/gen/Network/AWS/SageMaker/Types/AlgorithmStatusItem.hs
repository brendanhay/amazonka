{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatusItem
  ( AlgorithmStatusItem (..),

    -- * Smart constructor
    mkAlgorithmStatusItem,

    -- * Lenses
    asiStatus,
    asiFailureReason,
    asiName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.DetailedAlgorithmStatus

-- | Represents the overall status of an algorithm.
--
-- /See:/ 'mkAlgorithmStatusItem' smart constructor.
data AlgorithmStatusItem = AlgorithmStatusItem'
  { -- | The current status.
    status :: DetailedAlgorithmStatus,
    -- | if the overall status is @Failed@ , the reason for the failure.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The name of the algorithm for which the overall status is being reported.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlgorithmStatusItem' with the minimum fields required to make a request.
--
-- * 'status' - The current status.
-- * 'failureReason' - if the overall status is @Failed@ , the reason for the failure.
-- * 'name' - The name of the algorithm for which the overall status is being reported.
mkAlgorithmStatusItem ::
  -- | 'status'
  DetailedAlgorithmStatus ->
  -- | 'name'
  Lude.Text ->
  AlgorithmStatusItem
mkAlgorithmStatusItem pStatus_ pName_ =
  AlgorithmStatusItem'
    { status = pStatus_,
      failureReason = Lude.Nothing,
      name = pName_
    }

-- | The current status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiStatus :: Lens.Lens' AlgorithmStatusItem DetailedAlgorithmStatus
asiStatus = Lens.lens (status :: AlgorithmStatusItem -> DetailedAlgorithmStatus) (\s a -> s {status = a} :: AlgorithmStatusItem)
{-# DEPRECATED asiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | if the overall status is @Failed@ , the reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiFailureReason :: Lens.Lens' AlgorithmStatusItem (Lude.Maybe Lude.Text)
asiFailureReason = Lens.lens (failureReason :: AlgorithmStatusItem -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: AlgorithmStatusItem)
{-# DEPRECATED asiFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the algorithm for which the overall status is being reported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiName :: Lens.Lens' AlgorithmStatusItem Lude.Text
asiName = Lens.lens (name :: AlgorithmStatusItem -> Lude.Text) (\s a -> s {name = a} :: AlgorithmStatusItem)
{-# DEPRECATED asiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AlgorithmStatusItem where
  parseJSON =
    Lude.withObject
      "AlgorithmStatusItem"
      ( \x ->
          AlgorithmStatusItem'
            Lude.<$> (x Lude..: "Status")
            Lude.<*> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..: "Name")
      )
