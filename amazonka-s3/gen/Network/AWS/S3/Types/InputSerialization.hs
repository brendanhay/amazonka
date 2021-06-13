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
-- Module      : Network.AWS.S3.Types.InputSerialization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InputSerialization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CSVInput
import Network.AWS.S3.Types.CompressionType
import Network.AWS.S3.Types.JSONInput
import Network.AWS.S3.Types.ParquetInput

-- | Describes the serialization format of the object.
--
-- /See:/ 'newInputSerialization' smart constructor.
data InputSerialization = InputSerialization'
  { -- | Specifies Parquet as object\'s input serialization format.
    parquet :: Prelude.Maybe ParquetInput,
    -- | Describes the serialization of a CSV-encoded object.
    csv :: Prelude.Maybe CSVInput,
    -- | Specifies JSON as object\'s input serialization format.
    json :: Prelude.Maybe JSONInput,
    -- | Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
    -- Default Value: NONE.
    compressionType :: Prelude.Maybe CompressionType
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
-- 'parquet', 'inputSerialization_parquet' - Specifies Parquet as object\'s input serialization format.
--
-- 'csv', 'inputSerialization_csv' - Describes the serialization of a CSV-encoded object.
--
-- 'json', 'inputSerialization_json' - Specifies JSON as object\'s input serialization format.
--
-- 'compressionType', 'inputSerialization_compressionType' - Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
-- Default Value: NONE.
newInputSerialization ::
  InputSerialization
newInputSerialization =
  InputSerialization'
    { parquet = Prelude.Nothing,
      csv = Prelude.Nothing,
      json = Prelude.Nothing,
      compressionType = Prelude.Nothing
    }

-- | Specifies Parquet as object\'s input serialization format.
inputSerialization_parquet :: Lens.Lens' InputSerialization (Prelude.Maybe ParquetInput)
inputSerialization_parquet = Lens.lens (\InputSerialization' {parquet} -> parquet) (\s@InputSerialization' {} a -> s {parquet = a} :: InputSerialization)

-- | Describes the serialization of a CSV-encoded object.
inputSerialization_csv :: Lens.Lens' InputSerialization (Prelude.Maybe CSVInput)
inputSerialization_csv = Lens.lens (\InputSerialization' {csv} -> csv) (\s@InputSerialization' {} a -> s {csv = a} :: InputSerialization)

-- | Specifies JSON as object\'s input serialization format.
inputSerialization_json :: Lens.Lens' InputSerialization (Prelude.Maybe JSONInput)
inputSerialization_json = Lens.lens (\InputSerialization' {json} -> json) (\s@InputSerialization' {} a -> s {json = a} :: InputSerialization)

-- | Specifies object\'s compression format. Valid values: NONE, GZIP, BZIP2.
-- Default Value: NONE.
inputSerialization_compressionType :: Lens.Lens' InputSerialization (Prelude.Maybe CompressionType)
inputSerialization_compressionType = Lens.lens (\InputSerialization' {compressionType} -> compressionType) (\s@InputSerialization' {} a -> s {compressionType = a} :: InputSerialization)

instance Prelude.Hashable InputSerialization

instance Prelude.NFData InputSerialization

instance Core.ToXML InputSerialization where
  toXML InputSerialization' {..} =
    Prelude.mconcat
      [ "Parquet" Core.@= parquet,
        "CSV" Core.@= csv,
        "JSON" Core.@= json,
        "CompressionType" Core.@= compressionType
      ]
