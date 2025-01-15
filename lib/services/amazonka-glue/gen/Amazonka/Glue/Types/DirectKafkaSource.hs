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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DirectKafkaSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.KafkaStreamingSourceOptions
import Amazonka.Glue.Types.StreamingDataPreviewOptions
import qualified Amazonka.Prelude as Prelude

-- | Specifies an Apache Kafka data store.
--
-- /See:/ 'newDirectKafkaSource' smart constructor.
data DirectKafkaSource = DirectKafkaSource'
  { -- | Specifies options related to data preview for viewing a sample of your
    -- data.
    dataPreviewOptions :: Prelude.Maybe StreamingDataPreviewOptions,
    -- | Whether to automatically determine the schema from the incoming data.
    detectSchema :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the streaming options.
    streamingOptions :: Prelude.Maybe KafkaStreamingSourceOptions,
    -- | The amount of time to spend processing each micro batch.
    windowSize :: Prelude.Maybe Prelude.Natural,
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
-- 'dataPreviewOptions', 'directKafkaSource_dataPreviewOptions' - Specifies options related to data preview for viewing a sample of your
-- data.
--
-- 'detectSchema', 'directKafkaSource_detectSchema' - Whether to automatically determine the schema from the incoming data.
--
-- 'streamingOptions', 'directKafkaSource_streamingOptions' - Specifies the streaming options.
--
-- 'windowSize', 'directKafkaSource_windowSize' - The amount of time to spend processing each micro batch.
--
-- 'name', 'directKafkaSource_name' - The name of the data store.
newDirectKafkaSource ::
  -- | 'name'
  Prelude.Text ->
  DirectKafkaSource
newDirectKafkaSource pName_ =
  DirectKafkaSource'
    { dataPreviewOptions =
        Prelude.Nothing,
      detectSchema = Prelude.Nothing,
      streamingOptions = Prelude.Nothing,
      windowSize = Prelude.Nothing,
      name = pName_
    }

-- | Specifies options related to data preview for viewing a sample of your
-- data.
directKafkaSource_dataPreviewOptions :: Lens.Lens' DirectKafkaSource (Prelude.Maybe StreamingDataPreviewOptions)
directKafkaSource_dataPreviewOptions = Lens.lens (\DirectKafkaSource' {dataPreviewOptions} -> dataPreviewOptions) (\s@DirectKafkaSource' {} a -> s {dataPreviewOptions = a} :: DirectKafkaSource)

-- | Whether to automatically determine the schema from the incoming data.
directKafkaSource_detectSchema :: Lens.Lens' DirectKafkaSource (Prelude.Maybe Prelude.Bool)
directKafkaSource_detectSchema = Lens.lens (\DirectKafkaSource' {detectSchema} -> detectSchema) (\s@DirectKafkaSource' {} a -> s {detectSchema = a} :: DirectKafkaSource)

-- | Specifies the streaming options.
directKafkaSource_streamingOptions :: Lens.Lens' DirectKafkaSource (Prelude.Maybe KafkaStreamingSourceOptions)
directKafkaSource_streamingOptions = Lens.lens (\DirectKafkaSource' {streamingOptions} -> streamingOptions) (\s@DirectKafkaSource' {} a -> s {streamingOptions = a} :: DirectKafkaSource)

-- | The amount of time to spend processing each micro batch.
directKafkaSource_windowSize :: Lens.Lens' DirectKafkaSource (Prelude.Maybe Prelude.Natural)
directKafkaSource_windowSize = Lens.lens (\DirectKafkaSource' {windowSize} -> windowSize) (\s@DirectKafkaSource' {} a -> s {windowSize = a} :: DirectKafkaSource)

-- | The name of the data store.
directKafkaSource_name :: Lens.Lens' DirectKafkaSource Prelude.Text
directKafkaSource_name = Lens.lens (\DirectKafkaSource' {name} -> name) (\s@DirectKafkaSource' {} a -> s {name = a} :: DirectKafkaSource)

instance Data.FromJSON DirectKafkaSource where
  parseJSON =
    Data.withObject
      "DirectKafkaSource"
      ( \x ->
          DirectKafkaSource'
            Prelude.<$> (x Data..:? "DataPreviewOptions")
            Prelude.<*> (x Data..:? "DetectSchema")
            Prelude.<*> (x Data..:? "StreamingOptions")
            Prelude.<*> (x Data..:? "WindowSize")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable DirectKafkaSource where
  hashWithSalt _salt DirectKafkaSource' {..} =
    _salt
      `Prelude.hashWithSalt` dataPreviewOptions
      `Prelude.hashWithSalt` detectSchema
      `Prelude.hashWithSalt` streamingOptions
      `Prelude.hashWithSalt` windowSize
      `Prelude.hashWithSalt` name

instance Prelude.NFData DirectKafkaSource where
  rnf DirectKafkaSource' {..} =
    Prelude.rnf dataPreviewOptions `Prelude.seq`
      Prelude.rnf detectSchema `Prelude.seq`
        Prelude.rnf streamingOptions `Prelude.seq`
          Prelude.rnf windowSize `Prelude.seq`
            Prelude.rnf name

instance Data.ToJSON DirectKafkaSource where
  toJSON DirectKafkaSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataPreviewOptions" Data..=)
              Prelude.<$> dataPreviewOptions,
            ("DetectSchema" Data..=) Prelude.<$> detectSchema,
            ("StreamingOptions" Data..=)
              Prelude.<$> streamingOptions,
            ("WindowSize" Data..=) Prelude.<$> windowSize,
            Prelude.Just ("Name" Data..= name)
          ]
      )
