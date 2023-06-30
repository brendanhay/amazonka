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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDataSourceConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobDataSourceConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobS3Data

-- |
--
-- /See:/ 'newVectorEnrichmentJobDataSourceConfigInput' smart constructor.
data VectorEnrichmentJobDataSourceConfigInput = VectorEnrichmentJobDataSourceConfigInput'
  { s3Data :: Prelude.Maybe VectorEnrichmentJobS3Data
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobDataSourceConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Data', 'vectorEnrichmentJobDataSourceConfigInput_s3Data' -
newVectorEnrichmentJobDataSourceConfigInput ::
  VectorEnrichmentJobDataSourceConfigInput
newVectorEnrichmentJobDataSourceConfigInput =
  VectorEnrichmentJobDataSourceConfigInput'
    { s3Data =
        Prelude.Nothing
    }

vectorEnrichmentJobDataSourceConfigInput_s3Data :: Lens.Lens' VectorEnrichmentJobDataSourceConfigInput (Prelude.Maybe VectorEnrichmentJobS3Data)
vectorEnrichmentJobDataSourceConfigInput_s3Data = Lens.lens (\VectorEnrichmentJobDataSourceConfigInput' {s3Data} -> s3Data) (\s@VectorEnrichmentJobDataSourceConfigInput' {} a -> s {s3Data = a} :: VectorEnrichmentJobDataSourceConfigInput)

instance
  Data.FromJSON
    VectorEnrichmentJobDataSourceConfigInput
  where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobDataSourceConfigInput"
      ( \x ->
          VectorEnrichmentJobDataSourceConfigInput'
            Prelude.<$> (x Data..:? "S3Data")
      )

instance
  Prelude.Hashable
    VectorEnrichmentJobDataSourceConfigInput
  where
  hashWithSalt
    _salt
    VectorEnrichmentJobDataSourceConfigInput' {..} =
      _salt `Prelude.hashWithSalt` s3Data

instance
  Prelude.NFData
    VectorEnrichmentJobDataSourceConfigInput
  where
  rnf VectorEnrichmentJobDataSourceConfigInput' {..} =
    Prelude.rnf s3Data

instance
  Data.ToJSON
    VectorEnrichmentJobDataSourceConfigInput
  where
  toJSON VectorEnrichmentJobDataSourceConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Data" Data..=) Prelude.<$> s3Data]
      )
