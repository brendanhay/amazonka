{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputUpdate where

import Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
import Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
import Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes updates to a specific input configuration (identified by the
-- @InputId@ of an application).
--
-- /See:/ 'newInputUpdate' smart constructor.
data InputUpdate = InputUpdate'
  { -- | Name prefix for in-application streams that Amazon Kinesis Analytics
    -- creates for the specific streaming source.
    namePrefixUpdate :: Prelude.Maybe Prelude.Text,
    -- | If an Amazon Kinesis Firehose delivery stream is the streaming source to
    -- be updated, provides an updated stream ARN and IAM role ARN.
    kinesisFirehoseInputUpdate :: Prelude.Maybe KinesisFirehoseInputUpdate,
    -- | If an Amazon Kinesis stream is the streaming source to be updated,
    -- provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
    kinesisStreamsInputUpdate :: Prelude.Maybe KinesisStreamsInputUpdate,
    -- | Describes updates for an input processing configuration.
    inputProcessingConfigurationUpdate :: Prelude.Maybe InputProcessingConfigurationUpdate,
    -- | Describes the data format on the streaming source, and how record
    -- elements on the streaming source map to columns of the in-application
    -- stream that is created.
    inputSchemaUpdate :: Prelude.Maybe InputSchemaUpdate,
    -- | Describes the parallelism updates (the number in-application streams
    -- Amazon Kinesis Analytics creates for the specific streaming source).
    inputParallelismUpdate :: Prelude.Maybe InputParallelismUpdate,
    -- | Input ID of the application input to be updated.
    inputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namePrefixUpdate', 'inputUpdate_namePrefixUpdate' - Name prefix for in-application streams that Amazon Kinesis Analytics
-- creates for the specific streaming source.
--
-- 'kinesisFirehoseInputUpdate', 'inputUpdate_kinesisFirehoseInputUpdate' - If an Amazon Kinesis Firehose delivery stream is the streaming source to
-- be updated, provides an updated stream ARN and IAM role ARN.
--
-- 'kinesisStreamsInputUpdate', 'inputUpdate_kinesisStreamsInputUpdate' - If an Amazon Kinesis stream is the streaming source to be updated,
-- provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
--
-- 'inputProcessingConfigurationUpdate', 'inputUpdate_inputProcessingConfigurationUpdate' - Describes updates for an input processing configuration.
--
-- 'inputSchemaUpdate', 'inputUpdate_inputSchemaUpdate' - Describes the data format on the streaming source, and how record
-- elements on the streaming source map to columns of the in-application
-- stream that is created.
--
-- 'inputParallelismUpdate', 'inputUpdate_inputParallelismUpdate' - Describes the parallelism updates (the number in-application streams
-- Amazon Kinesis Analytics creates for the specific streaming source).
--
-- 'inputId', 'inputUpdate_inputId' - Input ID of the application input to be updated.
newInputUpdate ::
  -- | 'inputId'
  Prelude.Text ->
  InputUpdate
newInputUpdate pInputId_ =
  InputUpdate'
    { namePrefixUpdate = Prelude.Nothing,
      kinesisFirehoseInputUpdate = Prelude.Nothing,
      kinesisStreamsInputUpdate = Prelude.Nothing,
      inputProcessingConfigurationUpdate = Prelude.Nothing,
      inputSchemaUpdate = Prelude.Nothing,
      inputParallelismUpdate = Prelude.Nothing,
      inputId = pInputId_
    }

-- | Name prefix for in-application streams that Amazon Kinesis Analytics
-- creates for the specific streaming source.
inputUpdate_namePrefixUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe Prelude.Text)
inputUpdate_namePrefixUpdate = Lens.lens (\InputUpdate' {namePrefixUpdate} -> namePrefixUpdate) (\s@InputUpdate' {} a -> s {namePrefixUpdate = a} :: InputUpdate)

-- | If an Amazon Kinesis Firehose delivery stream is the streaming source to
-- be updated, provides an updated stream ARN and IAM role ARN.
inputUpdate_kinesisFirehoseInputUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe KinesisFirehoseInputUpdate)
inputUpdate_kinesisFirehoseInputUpdate = Lens.lens (\InputUpdate' {kinesisFirehoseInputUpdate} -> kinesisFirehoseInputUpdate) (\s@InputUpdate' {} a -> s {kinesisFirehoseInputUpdate = a} :: InputUpdate)

-- | If an Amazon Kinesis stream is the streaming source to be updated,
-- provides an updated stream Amazon Resource Name (ARN) and IAM role ARN.
inputUpdate_kinesisStreamsInputUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe KinesisStreamsInputUpdate)
inputUpdate_kinesisStreamsInputUpdate = Lens.lens (\InputUpdate' {kinesisStreamsInputUpdate} -> kinesisStreamsInputUpdate) (\s@InputUpdate' {} a -> s {kinesisStreamsInputUpdate = a} :: InputUpdate)

-- | Describes updates for an input processing configuration.
inputUpdate_inputProcessingConfigurationUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputProcessingConfigurationUpdate)
inputUpdate_inputProcessingConfigurationUpdate = Lens.lens (\InputUpdate' {inputProcessingConfigurationUpdate} -> inputProcessingConfigurationUpdate) (\s@InputUpdate' {} a -> s {inputProcessingConfigurationUpdate = a} :: InputUpdate)

-- | Describes the data format on the streaming source, and how record
-- elements on the streaming source map to columns of the in-application
-- stream that is created.
inputUpdate_inputSchemaUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputSchemaUpdate)
inputUpdate_inputSchemaUpdate = Lens.lens (\InputUpdate' {inputSchemaUpdate} -> inputSchemaUpdate) (\s@InputUpdate' {} a -> s {inputSchemaUpdate = a} :: InputUpdate)

-- | Describes the parallelism updates (the number in-application streams
-- Amazon Kinesis Analytics creates for the specific streaming source).
inputUpdate_inputParallelismUpdate :: Lens.Lens' InputUpdate (Prelude.Maybe InputParallelismUpdate)
inputUpdate_inputParallelismUpdate = Lens.lens (\InputUpdate' {inputParallelismUpdate} -> inputParallelismUpdate) (\s@InputUpdate' {} a -> s {inputParallelismUpdate = a} :: InputUpdate)

-- | Input ID of the application input to be updated.
inputUpdate_inputId :: Lens.Lens' InputUpdate Prelude.Text
inputUpdate_inputId = Lens.lens (\InputUpdate' {inputId} -> inputId) (\s@InputUpdate' {} a -> s {inputId = a} :: InputUpdate)

instance Prelude.Hashable InputUpdate

instance Prelude.NFData InputUpdate

instance Prelude.ToJSON InputUpdate where
  toJSON InputUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NamePrefixUpdate" Prelude..=)
              Prelude.<$> namePrefixUpdate,
            ("KinesisFirehoseInputUpdate" Prelude..=)
              Prelude.<$> kinesisFirehoseInputUpdate,
            ("KinesisStreamsInputUpdate" Prelude..=)
              Prelude.<$> kinesisStreamsInputUpdate,
            ("InputProcessingConfigurationUpdate" Prelude..=)
              Prelude.<$> inputProcessingConfigurationUpdate,
            ("InputSchemaUpdate" Prelude..=)
              Prelude.<$> inputSchemaUpdate,
            ("InputParallelismUpdate" Prelude..=)
              Prelude.<$> inputParallelismUpdate,
            Prelude.Just ("InputId" Prelude..= inputId)
          ]
      )
