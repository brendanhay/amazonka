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
-- Module      : Amazonka.Glue.Types.S3HudiSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.S3HudiSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import Amazonka.Glue.Types.S3DirectSourceAdditionalOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a Hudi data source stored in Amazon S3.
--
-- /See:/ 'newS3HudiSource' smart constructor.
data S3HudiSource = S3HudiSource'
  { -- | Specifies additional connection options.
    additionalHudiOptions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies additional options for the connector.
    additionalOptions :: Prelude.Maybe S3DirectSourceAdditionalOptions,
    -- | Specifies the data schema for the Hudi source.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the Hudi source.
    name :: Prelude.Text,
    -- | A list of the Amazon S3 paths to read from.
    paths :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3HudiSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalHudiOptions', 's3HudiSource_additionalHudiOptions' - Specifies additional connection options.
--
-- 'additionalOptions', 's3HudiSource_additionalOptions' - Specifies additional options for the connector.
--
-- 'outputSchemas', 's3HudiSource_outputSchemas' - Specifies the data schema for the Hudi source.
--
-- 'name', 's3HudiSource_name' - The name of the Hudi source.
--
-- 'paths', 's3HudiSource_paths' - A list of the Amazon S3 paths to read from.
newS3HudiSource ::
  -- | 'name'
  Prelude.Text ->
  S3HudiSource
newS3HudiSource pName_ =
  S3HudiSource'
    { additionalHudiOptions =
        Prelude.Nothing,
      additionalOptions = Prelude.Nothing,
      outputSchemas = Prelude.Nothing,
      name = pName_,
      paths = Prelude.mempty
    }

-- | Specifies additional connection options.
s3HudiSource_additionalHudiOptions :: Lens.Lens' S3HudiSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
s3HudiSource_additionalHudiOptions = Lens.lens (\S3HudiSource' {additionalHudiOptions} -> additionalHudiOptions) (\s@S3HudiSource' {} a -> s {additionalHudiOptions = a} :: S3HudiSource) Prelude.. Lens.mapping Lens.coerced

-- | Specifies additional options for the connector.
s3HudiSource_additionalOptions :: Lens.Lens' S3HudiSource (Prelude.Maybe S3DirectSourceAdditionalOptions)
s3HudiSource_additionalOptions = Lens.lens (\S3HudiSource' {additionalOptions} -> additionalOptions) (\s@S3HudiSource' {} a -> s {additionalOptions = a} :: S3HudiSource)

-- | Specifies the data schema for the Hudi source.
s3HudiSource_outputSchemas :: Lens.Lens' S3HudiSource (Prelude.Maybe [GlueSchema])
s3HudiSource_outputSchemas = Lens.lens (\S3HudiSource' {outputSchemas} -> outputSchemas) (\s@S3HudiSource' {} a -> s {outputSchemas = a} :: S3HudiSource) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Hudi source.
s3HudiSource_name :: Lens.Lens' S3HudiSource Prelude.Text
s3HudiSource_name = Lens.lens (\S3HudiSource' {name} -> name) (\s@S3HudiSource' {} a -> s {name = a} :: S3HudiSource)

-- | A list of the Amazon S3 paths to read from.
s3HudiSource_paths :: Lens.Lens' S3HudiSource [Prelude.Text]
s3HudiSource_paths = Lens.lens (\S3HudiSource' {paths} -> paths) (\s@S3HudiSource' {} a -> s {paths = a} :: S3HudiSource) Prelude.. Lens.coerced

instance Data.FromJSON S3HudiSource where
  parseJSON =
    Data.withObject
      "S3HudiSource"
      ( \x ->
          S3HudiSource'
            Prelude.<$> ( x
                            Data..:? "AdditionalHudiOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AdditionalOptions")
            Prelude.<*> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..:? "Paths" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable S3HudiSource where
  hashWithSalt _salt S3HudiSource' {..} =
    _salt
      `Prelude.hashWithSalt` additionalHudiOptions
      `Prelude.hashWithSalt` additionalOptions
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` paths

instance Prelude.NFData S3HudiSource where
  rnf S3HudiSource' {..} =
    Prelude.rnf additionalHudiOptions
      `Prelude.seq` Prelude.rnf additionalOptions
      `Prelude.seq` Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf paths

instance Data.ToJSON S3HudiSource where
  toJSON S3HudiSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalHudiOptions" Data..=)
              Prelude.<$> additionalHudiOptions,
            ("AdditionalOptions" Data..=)
              Prelude.<$> additionalOptions,
            ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Paths" Data..= paths)
          ]
      )
