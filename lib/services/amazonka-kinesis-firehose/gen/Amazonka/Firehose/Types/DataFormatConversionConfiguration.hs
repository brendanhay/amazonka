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
-- Module      : Amazonka.Firehose.Types.DataFormatConversionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DataFormatConversionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.InputFormatConfiguration
import Amazonka.Firehose.Types.OutputFormatConfiguration
import Amazonka.Firehose.Types.SchemaConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies that you want Kinesis Data Firehose to convert data from the
-- JSON format to the Parquet or ORC format before writing it to Amazon S3.
-- Kinesis Data Firehose uses the serializer and deserializer that you
-- specify, in addition to the column information from the Amazon Web
-- Services Glue table, to deserialize your input data from JSON and then
-- serialize it to the Parquet or ORC format. For more information, see
-- <https://docs.aws.amazon.com/firehose/latest/dev/record-format-conversion.html Kinesis Data Firehose Record Format Conversion>.
--
-- /See:/ 'newDataFormatConversionConfiguration' smart constructor.
data DataFormatConversionConfiguration = DataFormatConversionConfiguration'
  { -- | Defaults to @true@. Set it to @false@ if you want to disable format
    -- conversion while preserving the configuration details.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the deserializer that you want Kinesis Data Firehose to use to
    -- convert the format of your data from JSON. This parameter is required if
    -- @Enabled@ is set to true.
    inputFormatConfiguration :: Prelude.Maybe InputFormatConfiguration,
    -- | Specifies the serializer that you want Kinesis Data Firehose to use to
    -- convert the format of your data to the Parquet or ORC format. This
    -- parameter is required if @Enabled@ is set to true.
    outputFormatConfiguration :: Prelude.Maybe OutputFormatConfiguration,
    -- | Specifies the Amazon Web Services Glue Data Catalog table that contains
    -- the column information. This parameter is required if @Enabled@ is set
    -- to true.
    schemaConfiguration :: Prelude.Maybe SchemaConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataFormatConversionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'dataFormatConversionConfiguration_enabled' - Defaults to @true@. Set it to @false@ if you want to disable format
-- conversion while preserving the configuration details.
--
-- 'inputFormatConfiguration', 'dataFormatConversionConfiguration_inputFormatConfiguration' - Specifies the deserializer that you want Kinesis Data Firehose to use to
-- convert the format of your data from JSON. This parameter is required if
-- @Enabled@ is set to true.
--
-- 'outputFormatConfiguration', 'dataFormatConversionConfiguration_outputFormatConfiguration' - Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data to the Parquet or ORC format. This
-- parameter is required if @Enabled@ is set to true.
--
-- 'schemaConfiguration', 'dataFormatConversionConfiguration_schemaConfiguration' - Specifies the Amazon Web Services Glue Data Catalog table that contains
-- the column information. This parameter is required if @Enabled@ is set
-- to true.
newDataFormatConversionConfiguration ::
  DataFormatConversionConfiguration
newDataFormatConversionConfiguration =
  DataFormatConversionConfiguration'
    { enabled =
        Prelude.Nothing,
      inputFormatConfiguration =
        Prelude.Nothing,
      outputFormatConfiguration =
        Prelude.Nothing,
      schemaConfiguration = Prelude.Nothing
    }

-- | Defaults to @true@. Set it to @false@ if you want to disable format
-- conversion while preserving the configuration details.
dataFormatConversionConfiguration_enabled :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe Prelude.Bool)
dataFormatConversionConfiguration_enabled = Lens.lens (\DataFormatConversionConfiguration' {enabled} -> enabled) (\s@DataFormatConversionConfiguration' {} a -> s {enabled = a} :: DataFormatConversionConfiguration)

-- | Specifies the deserializer that you want Kinesis Data Firehose to use to
-- convert the format of your data from JSON. This parameter is required if
-- @Enabled@ is set to true.
dataFormatConversionConfiguration_inputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe InputFormatConfiguration)
dataFormatConversionConfiguration_inputFormatConfiguration = Lens.lens (\DataFormatConversionConfiguration' {inputFormatConfiguration} -> inputFormatConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {inputFormatConfiguration = a} :: DataFormatConversionConfiguration)

-- | Specifies the serializer that you want Kinesis Data Firehose to use to
-- convert the format of your data to the Parquet or ORC format. This
-- parameter is required if @Enabled@ is set to true.
dataFormatConversionConfiguration_outputFormatConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe OutputFormatConfiguration)
dataFormatConversionConfiguration_outputFormatConfiguration = Lens.lens (\DataFormatConversionConfiguration' {outputFormatConfiguration} -> outputFormatConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {outputFormatConfiguration = a} :: DataFormatConversionConfiguration)

-- | Specifies the Amazon Web Services Glue Data Catalog table that contains
-- the column information. This parameter is required if @Enabled@ is set
-- to true.
dataFormatConversionConfiguration_schemaConfiguration :: Lens.Lens' DataFormatConversionConfiguration (Prelude.Maybe SchemaConfiguration)
dataFormatConversionConfiguration_schemaConfiguration = Lens.lens (\DataFormatConversionConfiguration' {schemaConfiguration} -> schemaConfiguration) (\s@DataFormatConversionConfiguration' {} a -> s {schemaConfiguration = a} :: DataFormatConversionConfiguration)

instance
  Data.FromJSON
    DataFormatConversionConfiguration
  where
  parseJSON =
    Data.withObject
      "DataFormatConversionConfiguration"
      ( \x ->
          DataFormatConversionConfiguration'
            Prelude.<$> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "InputFormatConfiguration")
            Prelude.<*> (x Data..:? "OutputFormatConfiguration")
            Prelude.<*> (x Data..:? "SchemaConfiguration")
      )

instance
  Prelude.Hashable
    DataFormatConversionConfiguration
  where
  hashWithSalt
    _salt
    DataFormatConversionConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` enabled
        `Prelude.hashWithSalt` inputFormatConfiguration
        `Prelude.hashWithSalt` outputFormatConfiguration
        `Prelude.hashWithSalt` schemaConfiguration

instance
  Prelude.NFData
    DataFormatConversionConfiguration
  where
  rnf DataFormatConversionConfiguration' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf inputFormatConfiguration `Prelude.seq`
        Prelude.rnf outputFormatConfiguration `Prelude.seq`
          Prelude.rnf schemaConfiguration

instance
  Data.ToJSON
    DataFormatConversionConfiguration
  where
  toJSON DataFormatConversionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            ("InputFormatConfiguration" Data..=)
              Prelude.<$> inputFormatConfiguration,
            ("OutputFormatConfiguration" Data..=)
              Prelude.<$> outputFormatConfiguration,
            ("SchemaConfiguration" Data..=)
              Prelude.<$> schemaConfiguration
          ]
      )
