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
-- Module      : Amazonka.Glue.Types.DirectKafkaSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DirectKafkaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.KafkaStreamingSourceOptions
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Apache Kafka data store.
--
-- /See:/ 'newDirectKafkaSource' smart constructor.
data DirectKafkaSource = DirectKafkaSource'
  { -- | The amount of time to spend processing each micro batch.
    windowSize :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the streaming options.
    streamingOptions :: Prelude.Maybe KafkaStreamingSourceOptions,
    -- | Whether to automatically determine the schema from the incoming data.
    detectSchema :: Prelude.Maybe Prelude.Bool,
    -- | Specifies options related to data preview for viewing a sample of your
    -- data.
    dataPreviewOptions :: Prelude.Maybe StreamingDataPreviewOptions,
    -- | The name of the data store.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectKafkaSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'windowSize', 'directKafkaSource_windowSize' - The amount of time to spend processing each micro batch.
--
-- 'streamingOptions', 'directKafkaSource_streamingOptions' - Specifies the streaming options.
--
-- 'detectSchema', 'directKafkaSource_detectSchema' - Whether to automatically determine the schema from the incoming data.
--
-- 'dataPreviewOptions', 'directKafkaSource_dataPreviewOptions' - Specifies options related to data preview for viewing a sample of your
-- data.
--
-- 'name', 'directKafkaSource_name' - The name of the data store.
newDirectKafkaSource ::
  -- | 'name'
  Prelude.Text ->
  DirectKafkaSource
newDirectKafkaSource pName_ =
  DirectKafkaSource'
    { windowSize = Prelude.Nothing,
      streamingOptions = Prelude.Nothing,
      detectSchema = Prelude.Nothing,
      dataPreviewOptions = Prelude.Nothing,
      name = pName_
    }

-- | The amount of time to spend processing each micro batch.
directKafkaSource_windowSize :: Lens.Lens' DirectKafkaSource (Prelude.Maybe Prelude.Natural)
directKafkaSource_windowSize = Lens.lens (\DirectKafkaSource' {windowSize} -> windowSize) (\s@DirectKafkaSource' {} a -> s {windowSize = a} :: DirectKafkaSource)

-- | Specifies the streaming options.
directKafkaSource_streamingOptions :: Lens.Lens' DirectKafkaSource (Prelude.Maybe KafkaStreamingSourceOptions)
directKafkaSource_streamingOptions = Lens.lens (\DirectKafkaSource' {streamingOptions} -> streamingOptions) (\s@DirectKafkaSource' {} a -> s {streamingOptions = a} :: DirectKafkaSource)

-- | Whether to automatically determine the schema from the incoming data.
directKafkaSource_detectSchema :: Lens.Lens' DirectKafkaSource (Prelude.Maybe Prelude.Bool)
directKafkaSource_detectSchema = Lens.lens (\DirectKafkaSource' {detectSchema} -> detectSchema) (\s@DirectKafkaSource' {} a -> s {detectSchema = a} :: DirectKafkaSource)

-- | Specifies options related to data preview for viewing a sample of your
-- data.
directKafkaSource_dataPreviewOptions :: Lens.Lens' DirectKafkaSource (Prelude.Maybe StreamingDataPreviewOptions)
directKafkaSource_dataPreviewOptions = Lens.lens (\DirectKafkaSource' {dataPreviewOptions} -> dataPreviewOptions) (\s@DirectKafkaSource' {} a -> s {dataPreviewOptions = a} :: DirectKafkaSource)

-- | The name of the data store.
directKafkaSource_name :: Lens.Lens' DirectKafkaSource Prelude.Text
directKafkaSource_name = Lens.lens (\DirectKafkaSource' {name} -> name) (\s@DirectKafkaSource' {} a -> s {name = a} :: DirectKafkaSource)

instance Core.FromJSON DirectKafkaSource where
  parseJSON =
    Core.withObject
      "DirectKafkaSource"
      ( \x ->
          DirectKafkaSource'
            Prelude.<$> (x Core..:? "WindowSize")
            Prelude.<*> (x Core..:? "StreamingOptions")
            Prelude.<*> (x Core..:? "DetectSchema")
            Prelude.<*> (x Core..:? "DataPreviewOptions")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable DirectKafkaSource where
  hashWithSalt _salt DirectKafkaSource' {..} =
    _salt `Prelude.hashWithSalt` windowSize
      `Prelude.hashWithSalt` streamingOptions
      `Prelude.hashWithSalt` detectSchema
      `Prelude.hashWithSalt` dataPreviewOptions
      `Prelude.hashWithSalt` name

instance Prelude.NFData DirectKafkaSource where
  rnf DirectKafkaSource' {..} =
    Prelude.rnf windowSize
      `Prelude.seq` Prelude.rnf streamingOptions
      `Prelude.seq` Prelude.rnf detectSchema
      `Prelude.seq` Prelude.rnf dataPreviewOptions
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON DirectKafkaSource where
  toJSON DirectKafkaSource' {..} =
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
