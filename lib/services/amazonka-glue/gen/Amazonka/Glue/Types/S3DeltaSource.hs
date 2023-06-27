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
-- Module      : Amazonka.Glue.Types.S3DeltaSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3DeltaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Delta Lake data source stored in Amazon S3.
--
-- /See:/ 'newS3DeltaSource' smart constructor.
data S3DeltaSource = S3DeltaSource'
  { -- | Specifies additional connection options.
    additionalDeltaOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies additional options for the connector.
    additionalOptions :: Prelude.Maybe S3DirectSourceAdditionalOptions,
    -- | Specifies the data schema for the Delta Lake source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the Delta Lake source.
    name :: Prelude.Text,
    -- | A list of the Amazon S3 paths to read from.
    paths :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DeltaSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalDeltaOptions', 's3DeltaSource_additionalDeltaOptions' - Specifies additional connection options.
--
-- 'additionalOptions', 's3DeltaSource_additionalOptions' - Specifies additional options for the connector.
--
-- 'outputSchemas', 's3DeltaSource_outputSchemas' - Specifies the data schema for the Delta Lake source.
--
-- 'name', 's3DeltaSource_name' - The name of the Delta Lake source.
--
-- 'paths', 's3DeltaSource_paths' - A list of the Amazon S3 paths to read from.
newS3DeltaSource ::
  -- | 'name'
  Prelude.Text ->
  S3DeltaSource
newS3DeltaSource pName_ =
  S3DeltaSource'
    { additionalDeltaOptions =
        Prelude.Nothing,
      additionalOptions = Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      paths = Prelude.mempty
    }

-- | Specifies additional connection options.
s3DeltaSource_additionalDeltaOptions :: Lens.Lens' S3DeltaSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3DeltaSource_additionalDeltaOptions = Lens.lens (\S3DeltaSource' {additionalDeltaOptions} -> additionalDeltaOptions) (\s@S3DeltaSource' {} a -> s {additionalDeltaOptions = a} :: S3DeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies additional options for the connector.
s3DeltaSource_additionalOptions :: Lens.Lens' S3DeltaSource (Prelude.Maybe S3DirectSourceAdditionalOptions)
s3DeltaSource_additionalOptions = Lens.lens (\S3DeltaSource' {additionalOptions} -> additionalOptions) (\s@S3DeltaSource' {} a -> s {additionalOptions = a} :: S3DeltaSource)

-- | Specifies the data schema for the Delta Lake source.
s3DeltaSource_outputSchemas :: Lens.Lens' S3DeltaSource (Prelude.Maybe [GlueSchema])
s3DeltaSource_outputSchemas = Lens.lens (\S3DeltaSource' {outputSchemas} -> outputSchemas) (\s@S3DeltaSource' {} a -> s {outputSchemas = a} :: S3DeltaSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Delta Lake source.
s3DeltaSource_name :: Lens.Lens' S3DeltaSource Prelude.Text
s3DeltaSource_name = Lens.lens (\S3DeltaSource' {name} -> name) (\s@S3DeltaSource' {} a -> s {name = a} :: S3DeltaSource)

-- | A list of the Amazon S3 paths to read from.
s3DeltaSource_paths :: Lens.Lens' S3DeltaSource [Prelude.Text]
s3DeltaSource_paths = Lens.lens (\S3DeltaSource' {paths} -> paths) (\s@S3DeltaSource' {} a -> s {paths = a} :: S3DeltaSource) Prelude.. Lens.coerced

instance Data.FromJSON S3DeltaSource where
  parseJSON =
    Data.withObject
      "S3DeltaSource"
      ( \x ->
          S3DeltaSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalDeltaOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..:? "Paths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable S3DeltaSource where
  hashWithSalt _salt S3DeltaSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalDeltaOptions
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` paths

instance Prelude.NFData S3DeltaSource where
  rnf S3DeltaSource' {..} =
    Prelude.rnf additionalDeltaOptions
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf paths

instance Data.ToJSON S3DeltaSource where
  toJSON S3DeltaSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalDeltaOptions" Data..=)
              Prelude.<$> additionalDeltaOptions,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Paths" Data..= paths)
          ]
      )
