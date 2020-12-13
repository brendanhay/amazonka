{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatusDetails
  ( AlgorithmStatusDetails (..),

    -- * Smart constructor
    mkAlgorithmStatusDetails,

    -- * Lenses
    asdImageScanStatuses,
    asdValidationStatuses,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AlgorithmStatusItem

-- | Specifies the validation and image scan statuses of the algorithm.
--
-- /See:/ 'mkAlgorithmStatusDetails' smart constructor.
data AlgorithmStatusDetails = AlgorithmStatusDetails'
  { -- | The status of the scan of the algorithm's Docker image container.
    imageScanStatuses :: Lude.Maybe [AlgorithmStatusItem],
    -- | The status of algorithm validation.
    validationStatuses :: Lude.Maybe [AlgorithmStatusItem]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlgorithmStatusDetails' with the minimum fields required to make a request.
--
-- * 'imageScanStatuses' - The status of the scan of the algorithm's Docker image container.
-- * 'validationStatuses' - The status of algorithm validation.
mkAlgorithmStatusDetails ::
  AlgorithmStatusDetails
mkAlgorithmStatusDetails =
  AlgorithmStatusDetails'
    { imageScanStatuses = Lude.Nothing,
      validationStatuses = Lude.Nothing
    }

-- | The status of the scan of the algorithm's Docker image container.
--
-- /Note:/ Consider using 'imageScanStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdImageScanStatuses :: Lens.Lens' AlgorithmStatusDetails (Lude.Maybe [AlgorithmStatusItem])
asdImageScanStatuses = Lens.lens (imageScanStatuses :: AlgorithmStatusDetails -> Lude.Maybe [AlgorithmStatusItem]) (\s a -> s {imageScanStatuses = a} :: AlgorithmStatusDetails)
{-# DEPRECATED asdImageScanStatuses "Use generic-lens or generic-optics with 'imageScanStatuses' instead." #-}

-- | The status of algorithm validation.
--
-- /Note:/ Consider using 'validationStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asdValidationStatuses :: Lens.Lens' AlgorithmStatusDetails (Lude.Maybe [AlgorithmStatusItem])
asdValidationStatuses = Lens.lens (validationStatuses :: AlgorithmStatusDetails -> Lude.Maybe [AlgorithmStatusItem]) (\s a -> s {validationStatuses = a} :: AlgorithmStatusDetails)
{-# DEPRECATED asdValidationStatuses "Use generic-lens or generic-optics with 'validationStatuses' instead." #-}

instance Lude.FromJSON AlgorithmStatusDetails where
  parseJSON =
    Lude.withObject
      "AlgorithmStatusDetails"
      ( \x ->
          AlgorithmStatusDetails'
            Lude.<$> (x Lude..:? "ImageScanStatuses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ValidationStatuses" Lude..!= Lude.mempty)
      )
