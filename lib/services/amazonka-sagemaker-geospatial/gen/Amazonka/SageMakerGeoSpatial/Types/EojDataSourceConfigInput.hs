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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.EojDataSourceConfigInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.EojDataSourceConfigInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.S3DataInput

-- |
--
-- /See:/ 'newEojDataSourceConfigInput' smart constructor.
data EojDataSourceConfigInput = EojDataSourceConfigInput'
  { s3Data :: Prelude.Maybe S3DataInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EojDataSourceConfigInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Data', 'eojDataSourceConfigInput_s3Data' -
newEojDataSourceConfigInput ::
  EojDataSourceConfigInput
newEojDataSourceConfigInput =
  EojDataSourceConfigInput' {s3Data = Prelude.Nothing}

eojDataSourceConfigInput_s3Data :: Lens.Lens' EojDataSourceConfigInput (Prelude.Maybe S3DataInput)
eojDataSourceConfigInput_s3Data = Lens.lens (\EojDataSourceConfigInput' {s3Data} -> s3Data) (\s@EojDataSourceConfigInput' {} a -> s {s3Data = a} :: EojDataSourceConfigInput)

instance Data.FromJSON EojDataSourceConfigInput where
  parseJSON =
    Data.withObject
      "EojDataSourceConfigInput"
      ( \x ->
          EojDataSourceConfigInput'
            Prelude.<$> (x Data..:? "S3Data")
      )

instance Prelude.Hashable EojDataSourceConfigInput where
  hashWithSalt _salt EojDataSourceConfigInput' {..} =
    _salt `Prelude.hashWithSalt` s3Data

instance Prelude.NFData EojDataSourceConfigInput where
  rnf EojDataSourceConfigInput' {..} =
    Prelude.rnf s3Data

instance Data.ToJSON EojDataSourceConfigInput where
  toJSON EojDataSourceConfigInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [("S3Data" Data..=) Prelude.<$> s3Data]
      )
