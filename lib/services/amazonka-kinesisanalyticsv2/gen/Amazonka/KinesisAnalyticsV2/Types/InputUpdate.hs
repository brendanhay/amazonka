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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.InputParallelismUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputProcessingConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.InputSchemaUpdate
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputUpdate
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsInputUpdate
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, describes updates to
-- a specific input configuration (identified by the @InputId@ of an
-- application).
--
-- /See:/ 'newInputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { -- | Describes the parallelism updates (the number of in-application streams
    -- Kinesis Data Analytics creates for the specific streaming source).
    inputParallelismUpdate :: Prelude.Maybe InputParallelismUpdate,
    -- | Describes updates to an InputProcessingConfiguration.
    inputProcessingConfigurationUpdate :: Prelude.Maybe InputProcessingConfigurationUpdate,
    -- | Describes the data format on the streaming source, and how record
    -- elements on the streaming source map to columns of the in-application
    -- stream that is created.
    inputSchemaUpdate :: Prelude.Maybe InputSchemaUpdate,
    -- | If a Kinesis Data Firehose delivery stream is the streaming source to be
    -- updated, provides an updated stream ARN.
    kinesisFirehoseInputUpdate :: Prelude.Maybe KinesisFirehoseInputUpdate,
    -- | If a Kinesis data stream is the streaming source to be updated, provides
    -- an updated stream Amazon Resource Name (ARN).
    kinesisStreamsInputUpdate :: Prelude.Maybe KinesisStreamsInputUpdate,
    -- | The name prefix for in-application streams that Kinesis Data Analytics
    -- creates for the specific streaming source.
    namePrefixUpdate :: Prelude.Maybe Prelude.Text,
    -- | The input ID of the application input to be updated.
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputParallelismUpdate', 'inputUpdate_inputParallelismUpdate' - Describes the parallelism updates (the number of in-application streams
-- Kinesis Data Analytics creates for the specific streaming source).
--
-- 'inputProcessingConfigurationUpdate', 'inputUpdate_inputProcessingConfigurationUpdate' - Describes updates to an InputProcessingConfiguration.
--
-- 'inputSchemaUpdate', 'inputUpdate_inputSchemaUpdate' - Describes the data format on the streaming source, and how record
-- elements on the streaming source map to columns of the in-application
-- stream that is created.
--
-- 'kinesisFirehoseInputUpdate', 'inputUpdate_kinesisFirehoseInputUpdate' - If a Kinesis Data Firehose delivery stream is the streaming source to be
-- updated, provides an updated stream ARN.
--
-- 'kinesisStreamsInputUpdate', 'inputUpdate_kinesisStreamsInputUpdate' - If a Kinesis data stream is the streaming source to be updated, provides
-- an updated stream Amazon Resource Name (ARN).
--
-- 'namePrefixUpdate', 'inputUpdate_namePrefixUpdate' - The name prefix for in-application streams that Kinesis Data Analytics
-- creates for the specific streaming source.
--
-- 'inputId', 'inputUpdate_inputId' - The input ID of the application input to be updated.
newInputUpdate ::
  -- | 'inputId'
  Prelude.Text ->
  InputUpdate
newInputUpdate pInputId_ =
  InputUpdate'
    { inputParallelismUpdate =
        Prelude.Nothing,
      inputProcessingConfigurationUpdate = Prelude.Nothing,
      inputSchemaUpdate = Prelude.Nothing,
      kinesisFirehoseInputUpdate = Prelude.Nothing,
      kinesisStreamsInputUpdate = Prelude.Nothing,
      namePrefixUpdate = Prelude.Nothing,
      inputId = pInputId_
    }

-- | Describes the parallelism updates (the number of in-application streams
-- Kinesis Data Analytics creates for the specific streaming source).
inputUpdate_inputParallelismUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputParallelismUpdate)
inputUpdate_inputParallelismUpdate = Lens.lens (\InputUpdate' {inputParallelismUpdate} -> inputParallelismUpdate) (\s@InputUpdate' {} a -> s {inputParallelismUpdate = a} :: InputUpdate)

-- | Describes updates to an InputProcessingConfiguration.
inputUpdate_inputProcessingConfigurationUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputProcessingConfigurationUpdate)
inputUpdate_inputProcessingConfigurationUpdate = Lens.lens (\InputUpdate' {inputProcessingConfigurationUpdate} -> inputProcessingConfigurationUpdate) (\s@InputUpdate' {} a -> s {inputProcessingConfigurationUpdate = a} :: InputUpdate)

-- | Describes the data format on the streaming source, and how record
-- elements on the streaming source map to columns of the in-application
-- stream that is created.
inputUpdate_inputSchemaUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputSchemaUpdate)
inputUpdate_inputSchemaUpdate = Lens.lens (\InputUpdate' {inputSchemaUpdate} -> inputSchemaUpdate) (\s@InputUpdate' {} a -> s {inputSchemaUpdate = a} :: InputUpdate)

-- | If a Kinesis Data Firehose delivery stream is the streaming source to be
-- updated, provides an updated stream ARN.
inputUpdate_kinesisFirehoseInputUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe KinesisFirehoseInputUpdate)
inputUpdate_kinesisFirehoseInputUpdate = Lens.lens (\InputUpdate' {kinesisFirehoseInputUpdate} -> kinesisFirehoseInputUpdate) (\s@InputUpdate' {} a -> s {kinesisFirehoseInputUpdate = a} :: InputUpdate)

-- | If a Kinesis data stream is the streaming source to be updated, provides
-- an updated stream Amazon Resource Name (ARN).
inputUpdate_kinesisStreamsInputUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe KinesisStreamsInputUpdate)
inputUpdate_kinesisStreamsInputUpdate = Lens.lens (\InputUpdate' {kinesisStreamsInputUpdate} -> kinesisStreamsInputUpdate) (\s@InputUpdate' {} a -> s {kinesisStreamsInputUpdate = a} :: InputUpdate)

-- | The name prefix for in-application streams that Kinesis Data Analytics
-- creates for the specific streaming source.
inputUpdate_namePrefixUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe Prelude.Text)
inputUpdate_namePrefixUpdate = Lens.lens (\InputUpdate' {namePrefixUpdate} -> namePrefixUpdate) (\s@InputUpdate' {} a -> s {namePrefixUpdate = a} :: InputUpdate)

-- | The input ID of the application input to be updated.
inputUpdate_inputId :: Lens.Lens' InputUpdate Prelude.Text
inputUpdate_inputId = Lens.lens (\InputUpdate' {inputId} -> inputId) (\s@InputUpdate' {} a -> s {inputId = a} :: InputUpdate)

instance Prelude.Hashable InputUpdate where
  hashWithSalt _salt InputUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` inputParallelismUpdate
      `Prelude.hashWithSalt` inputProcessingConfigurationUpdate
      `Prelude.hashWithSalt` inputSchemaUpdate
      `Prelude.hashWithSalt` kinesisFirehoseInputUpdate
      `Prelude.hashWithSalt` kinesisStreamsInputUpdate
      `Prelude.hashWithSalt` namePrefixUpdate
      `Prelude.hashWithSalt` inputId

instance Prelude.NFData InputUpdate where
  rnf InputUpdate' {..} =
    Prelude.rnf inputParallelismUpdate
      `Prelude.seq` Prelude.rnf inputProcessingConfigurationUpdate
      `Prelude.seq` Prelude.rnf inputSchemaUpdate
      `Prelude.seq` Prelude.rnf kinesisFirehoseInputUpdate
      `Prelude.seq` Prelude.rnf kinesisStreamsInputUpdate
      `Prelude.seq` Prelude.rnf namePrefixUpdate
      `Prelude.seq` Prelude.rnf inputId

instance Data.ToJSON InputUpdate where
  toJSON InputUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputParallelismUpdate" Data..=)
              Prelude.<$> inputParallelismUpdate,
            ("InputProcessingConfigurationUpdate" Data..=)
              Prelude.<$> inputProcessingConfigurationUpdate,
            ("InputSchemaUpdate" Data..=)
              Prelude.<$> inputSchemaUpdate,
            ("KinesisFirehoseInputUpdate" Data..=)
              Prelude.<$> kinesisFirehoseInputUpdate,
            ("KinesisStreamsInputUpdate" Data..=)
              Prelude.<$> kinesisStreamsInputUpdate,
            ("NamePrefixUpdate" Data..=)
              Prelude.<$> namePrefixUpdate,
            Prelude.Just ("InputId" Data..= inputId)
          ]
      )
