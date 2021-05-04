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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageStatusDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageStatusDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ModelPackageStatusItem

-- | Specifies the validation and image scan statuses of the model package.
--
-- /See:/ 'newModelPackageStatusDetails' smart constructor.
data ModelPackageStatusDetails = ModelPackageStatusDetails'
  { -- | The status of the scan of the Docker image container for the model
    -- package.
    imageScanStatuses :: Prelude.Maybe [ModelPackageStatusItem],
    -- | The validation status of the model package.
    validationStatuses :: [ModelPackageStatusItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageStatusDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageScanStatuses', 'modelPackageStatusDetails_imageScanStatuses' - The status of the scan of the Docker image container for the model
-- package.
--
-- 'validationStatuses', 'modelPackageStatusDetails_validationStatuses' - The validation status of the model package.
newModelPackageStatusDetails ::
  ModelPackageStatusDetails
newModelPackageStatusDetails =
  ModelPackageStatusDetails'
    { imageScanStatuses =
        Prelude.Nothing,
      validationStatuses = Prelude.mempty
    }

-- | The status of the scan of the Docker image container for the model
-- package.
modelPackageStatusDetails_imageScanStatuses :: Lens.Lens' ModelPackageStatusDetails (Prelude.Maybe [ModelPackageStatusItem])
modelPackageStatusDetails_imageScanStatuses = Lens.lens (\ModelPackageStatusDetails' {imageScanStatuses} -> imageScanStatuses) (\s@ModelPackageStatusDetails' {} a -> s {imageScanStatuses = a} :: ModelPackageStatusDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The validation status of the model package.
modelPackageStatusDetails_validationStatuses :: Lens.Lens' ModelPackageStatusDetails [ModelPackageStatusItem]
modelPackageStatusDetails_validationStatuses = Lens.lens (\ModelPackageStatusDetails' {validationStatuses} -> validationStatuses) (\s@ModelPackageStatusDetails' {} a -> s {validationStatuses = a} :: ModelPackageStatusDetails) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ModelPackageStatusDetails where
  parseJSON =
    Prelude.withObject
      "ModelPackageStatusDetails"
      ( \x ->
          ModelPackageStatusDetails'
            Prelude.<$> ( x Prelude..:? "ImageScanStatuses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "ValidationStatuses"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ModelPackageStatusDetails

instance Prelude.NFData ModelPackageStatusDetails
