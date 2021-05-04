{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AlgorithmStatusItem

-- | Specifies the validation and image scan statuses of the algorithm.
--
-- /See:/ 'newAlgorithmStatusDetails' smart constructor.
data AlgorithmStatusDetails = AlgorithmStatusDetails'
  { -- | The status of algorithm validation.
    validationStatuses :: Prelude.Maybe [AlgorithmStatusItem],
    -- | The status of the scan of the algorithm\'s Docker image container.
    imageScanStatuses :: Prelude.Maybe [AlgorithmStatusItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      imageScanStatuses = Prelude.Nothing
    }

-- | The status of algorithm validation.
algorithmStatusDetails_validationStatuses :: Lens.Lens' AlgorithmStatusDetails (Prelude.Maybe [AlgorithmStatusItem])
algorithmStatusDetails_validationStatuses = Lens.lens (\AlgorithmStatusDetails' {validationStatuses} -> validationStatuses) (\s@AlgorithmStatusDetails' {} a -> s {validationStatuses = a} :: AlgorithmStatusDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The status of the scan of the algorithm\'s Docker image container.
algorithmStatusDetails_imageScanStatuses :: Lens.Lens' AlgorithmStatusDetails (Prelude.Maybe [AlgorithmStatusItem])
algorithmStatusDetails_imageScanStatuses = Lens.lens (\AlgorithmStatusDetails' {imageScanStatuses} -> imageScanStatuses) (\s@AlgorithmStatusDetails' {} a -> s {imageScanStatuses = a} :: AlgorithmStatusDetails) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON AlgorithmStatusDetails where
  parseJSON =
    Prelude.withObject
      "AlgorithmStatusDetails"
      ( \x ->
          AlgorithmStatusDetails'
            Prelude.<$> ( x Prelude..:? "ValidationStatuses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "ImageScanStatuses"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AlgorithmStatusDetails

instance Prelude.NFData AlgorithmStatusDetails
