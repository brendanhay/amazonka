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
-- Module      : Amazonka.Glue.Types.DirectKinesisSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DirectKinesisSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.KinesisStreamingSourceOptions
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies a direct Amazon Kinesis data source.
--
-- /See:/ 'newDirectKinesisSource' smart constructor.
data DirectKinesisSource = DirectKinesisSource'
  { -- | The amount of time to spend processing each micro batch.
    windowSize :: Prelude.Maybe Prelude.Natural,
    -- | Additional options for the Kinesis streaming data source.
    streamingOptions :: Prelude.Maybe KinesisStreamingSourceOptions,
    -- | Whether to automatically determine the schema from the incoming data.
    detectSchema :: Prelude.Maybe Prelude.Bool,
    -- | Additional options for data preview.
    dataPreviewOptions :: Prelude.Maybe StreamingDataPreviewOptions,
    -- | The name of the data source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectKinesisSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowSize', 'directKinesisSource_windowSize' - The amount of time to spend processing each micro batch.
--
-- 'streamingOptions', 'directKinesisSource_streamingOptions' - Additional options for the Kinesis streaming data source.
--
-- 'detectSchema', 'directKinesisSource_detectSchema' - Whether to automatically determine the schema from the incoming data.
--
-- 'dataPreviewOptions', 'directKinesisSource_dataPreviewOptions' - Additional options for data preview.
--
-- 'name', 'directKinesisSource_name' - The name of the data source.
newDirectKinesisSource ::
  -- | 'name'
  Prelude.Text ->
  DirectKinesisSource
newDirectKinesisSource pName_ =
  DirectKinesisSource'
    { windowSize = Prelude.Nothing,
      streamingOptions = Prelude.Nothing,
      detectSchema = Prelude.Nothing,
      dataPreviewOptions = Prelude.Nothing,
      name = pName_
    }

-- | The amount of time to spend processing each micro batch.
directKinesisSource_windowSize :: Lens.Lens' DirectKinesisSource (Prelude.Maybe Prelude.Natural)
directKinesisSource_windowSize = Lens.lens (\DirectKinesisSource' {windowSize} -> windowSize) (\s@DirectKinesisSource' {} a -> s {windowSize = a} :: DirectKinesisSource)

-- | Additional options for the Kinesis streaming data source.
directKinesisSource_streamingOptions :: Lens.Lens' DirectKinesisSource (Prelude.Maybe KinesisStreamingSourceOptions)
directKinesisSource_streamingOptions = Lens.lens (\DirectKinesisSource' {streamingOptions} -> streamingOptions) (\s@DirectKinesisSource' {} a -> s {streamingOptions = a} :: DirectKinesisSource)

-- | Whether to automatically determine the schema from the incoming data.
directKinesisSource_detectSchema :: Lens.Lens' DirectKinesisSource (Prelude.Maybe Prelude.Bool)
directKinesisSource_detectSchema = Lens.lens (\DirectKinesisSource' {detectSchema} -> detectSchema) (\s@DirectKinesisSource' {} a -> s {detectSchema = a} :: DirectKinesisSource)

-- | Additional options for data preview.
directKinesisSource_dataPreviewOptions :: Lens.Lens' DirectKinesisSource (Prelude.Maybe StreamingDataPreviewOptions)
directKinesisSource_dataPreviewOptions = Lens.lens (\DirectKinesisSource' {dataPreviewOptions} -> dataPreviewOptions) (\s@DirectKinesisSource' {} a -> s {dataPreviewOptions = a} :: DirectKinesisSource)

-- | The name of the data source.
directKinesisSource_name :: Lens.Lens' DirectKinesisSource Prelude.Text
directKinesisSource_name = Lens.lens (\DirectKinesisSource' {name} -> name) (\s@DirectKinesisSource' {} a -> s {name = a} :: DirectKinesisSource)

instance Core.FromJSON DirectKinesisSource where
  parseJSON =
    Core.withObject
      "DirectKinesisSource"
      ( \x ->
          DirectKinesisSource'
            Prelude.<$> (x Core..:? "WindowSize")
            Prelude.<*> (x Core..:? "StreamingOptions")
            Prelude.<*> (x Core..:? "DetectSchema")
            Prelude.<*> (x Core..:? "DataPreviewOptions")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable DirectKinesisSource where
  hashWithSalt _salt DirectKinesisSource' {..} =
    _salt `Prelude.hashWithSalt` windowSize
      `Prelude.hashWithSalt` streamingOptions
      `Prelude.hashWithSalt` detectSchema
      `Prelude.hashWithSalt` dataPreviewOptions
      `Prelude.hashWithSalt` name

instance Prelude.NFData DirectKinesisSource where
  rnf DirectKinesisSource' {..} =
    Prelude.rnf windowSize
      `Prelude.seq` Prelude.rnf streamingOptions
      `Prelude.seq` Prelude.rnf detectSchema
      `Prelude.seq` Prelude.rnf dataPreviewOptions
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON DirectKinesisSource where
  toJSON DirectKinesisSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WindowSize" Core..=) Prelude.<$> windowSize,
            ("StreamingOptions" Core..=)
              Prelude.<$> streamingOptions,
            ("DetectSchema" Core..=) Prelude.<$> detectSchema,
            ("DataPreviewOptions" Core..=)
              Prelude.<$> dataPreviewOptions,
            Prelude.Just ("Name" Core..= name)
          ]
      )
