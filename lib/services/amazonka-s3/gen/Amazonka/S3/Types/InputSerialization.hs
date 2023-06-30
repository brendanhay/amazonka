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
-- Module      : Amazonka.S3.Types.InputSerialization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InputSerialization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.CSVInput
import Amazonka.S3.Types.CompressionType
import Amazonka.S3.Types.JSONInput
import Amazonka.S3.Types.ParquetInput

-- | Describes the serialization format of the object.
--
-- /See:/ 'newInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { -- | Describes the serialization of a CSV-encoded object.
    csv :: Prelude.Maybe CSVInput,
    -- | Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
    -- Default Value: NONE.
    compressionType :: Prelude.Maybe CompressionType,
    -- | Specifies JSON as object\'s input serialization format.
    json :: Prelude.Maybe JSONInput,
    -- | Specifies Parquet as object\'s input serialization format.
    parquet :: Prelude.Maybe ParquetInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSerialization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'csv', 'inputSerialization_csv' - Describes the serialization of a CSV-encoded object.
--
-- 'compressionType', 'inputSerialization_compressionType' - Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
-- Default Value: NONE.
--
-- 'json', 'inputSerialization_json' - Specifies JSON as object\'s input serialization format.
--
-- 'parquet', 'inputSerialization_parquet' - Specifies Parquet as object\'s input serialization format.
newInputSerialization ::
  InputSerialization
newInputSerialization =
  InputSerialization'
    { csv = Prelude.Nothing,
      compressionType = Prelude.Nothing,
      json = Prelude.Nothing,
      parquet = Prelude.Nothing
    }

-- | Describes the serialization of a CSV-encoded object.
inputSerialization_csv :: Lens.Lens' InputSerialization (Prelude.Maybe CSVInput)
inputSerialization_csv = Lens.lens (\InputSerialization' {csv} -> csv) (\s@InputSerialization' {} a -> s {csv = a} :: InputSerialization)

-- | Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
-- Default Value: NONE.
inputSerialization_compressionType :: Lens.Lens' InputSerialization (Prelude.Maybe CompressionType)
inputSerialization_compressionType = Lens.lens (\InputSerialization' {compressionType} -> compressionType) (\s@InputSerialization' {} a -> s {compressionType = a} :: InputSerialization)

-- | Specifies JSON as object\'s input serialization format.
inputSerialization_json :: Lens.Lens' InputSerialization (Prelude.Maybe JSONInput)
inputSerialization_json = Lens.lens (\InputSerialization' {json} -> json) (\s@InputSerialization' {} a -> s {json = a} :: InputSerialization)

-- | Specifies Parquet as object\'s input serialization format.
inputSerialization_parquet :: Lens.Lens' InputSerialization (Prelude.Maybe ParquetInput)
inputSerialization_parquet = Lens.lens (\InputSerialization' {parquet} -> parquet) (\s@InputSerialization' {} a -> s {parquet = a} :: InputSerialization)

instance Prelude.Hashable InputSerialization where
  hashWithSalt _salt InputSerialization' {..} =
    _salt
      `Prelude.hashWithSalt` csv
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` json
      `Prelude.hashWithSalt` parquet

instance Prelude.NFData InputSerialization where
  rnf InputSerialization' {..} =
    Prelude.rnf csv
      `Prelude.seq` Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf json
      `Prelude.seq` Prelude.rnf parquet

instance Data.ToXML InputSerialization where
  toXML InputSerialization' {..} =
    Prelude.mconcat
      [ "CSV" Data.@= csv,
        "CompressionType" Data.@= compressionType,
        "JSON" Data.@= json,
        "Parquet" Data.@= parquet
      ]
