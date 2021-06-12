{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatusDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatusDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AlgorithmStatusItem

-- | Specifies the validation and image scan statuses of the algorithm.
--
-- /See:/ 'newAlgorithmStatusDetails' smart constructor.
data AlgorithmStatusDetails = AlgorithmStatusDetails'
  { -- | The status of algorithm validation.
    validationStatuses :: Core.Maybe [AlgorithmStatusItem],
    -- | The status of the scan of the algorithm\'s Docker image container.
    imageScanStatuses :: Core.Maybe [AlgorithmStatusItem]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AlgorithmStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationStatuses', 'algorithmStatusDetails_validationStatuses' - The status of algorithm validation.
--
-- 'imageScanStatuses', 'algorithmStatusDetails_imageScanStatuses' - The status of the scan of the algorithm\'s Docker image container.
newAlgorithmStatusDetails ::
  AlgorithmStatusDetails
newAlgorithmStatusDetails =
  AlgorithmStatusDetails'
    { validationStatuses =
        Core.Nothing,
      imageScanStatuses = Core.Nothing
    }

-- | The status of algorithm validation.
algorithmStatusDetails_validationStatuses :: Lens.Lens' AlgorithmStatusDetails (Core.Maybe [AlgorithmStatusItem])
algorithmStatusDetails_validationStatuses = Lens.lens (\AlgorithmStatusDetails' {validationStatuses} -> validationStatuses) (\s@AlgorithmStatusDetails' {} a -> s {validationStatuses = a} :: AlgorithmStatusDetails) Core.. Lens.mapping Lens._Coerce

-- | The status of the scan of the algorithm\'s Docker image container.
algorithmStatusDetails_imageScanStatuses :: Lens.Lens' AlgorithmStatusDetails (Core.Maybe [AlgorithmStatusItem])
algorithmStatusDetails_imageScanStatuses = Lens.lens (\AlgorithmStatusDetails' {imageScanStatuses} -> imageScanStatuses) (\s@AlgorithmStatusDetails' {} a -> s {imageScanStatuses = a} :: AlgorithmStatusDetails) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON AlgorithmStatusDetails where
  parseJSON =
    Core.withObject
      "AlgorithmStatusDetails"
      ( \x ->
          AlgorithmStatusDetails'
            Core.<$> ( x Core..:? "ValidationStatuses"
                         Core..!= Core.mempty
                     )
            Core.<*> ( x Core..:? "ImageScanStatuses"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable AlgorithmStatusDetails

instance Core.NFData AlgorithmStatusDetails
