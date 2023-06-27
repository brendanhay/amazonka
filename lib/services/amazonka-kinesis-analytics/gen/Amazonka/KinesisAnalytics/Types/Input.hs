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
-- Module      : Amazonka.KinesisAnalytics.Types.Input
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.Input where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.InputParallelism
import Amazonka.KinesisAnalytics.Types.InputProcessingConfiguration
import Amazonka.KinesisAnalytics.Types.KinesisFirehoseInput
import Amazonka.KinesisAnalytics.Types.KinesisStreamsInput
import Amazonka.KinesisAnalytics.Types.SourceSchema
import qualified Amazonka.Prelude as Prelude

-- | When you configure the application input, you specify the streaming
-- source, the in-application stream name that is created, and the mapping
-- between the two. For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- /See:/ 'newInput' smart constructor.
data Input = Input'
  { -- | Describes the number of in-application streams to create.
    --
    -- Data from your source is routed to these in-application input streams.
    --
    -- (see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
    inputParallelism :: Prelude.Maybe InputParallelism,
    -- | The
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
    -- for the input. An input processor transforms records as they are
    -- received from the stream, before the application\'s SQL code executes.
    -- Currently, the only input processing configuration available is
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
    inputProcessingConfiguration :: Prelude.Maybe InputProcessingConfiguration,
    -- | If the streaming source is an Amazon Kinesis Firehose delivery stream,
    -- identifies the delivery stream\'s ARN and an IAM role that enables
    -- Amazon Kinesis Analytics to access the stream on your behalf.
    --
    -- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
    -- required.
    kinesisFirehoseInput :: Prelude.Maybe KinesisFirehoseInput,
    -- | If the streaming source is an Amazon Kinesis stream, identifies the
    -- stream\'s Amazon Resource Name (ARN) and an IAM role that enables Amazon
    -- Kinesis Analytics to access the stream on your behalf.
    --
    -- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
    -- required.
    kinesisStreamsInput :: Prelude.Maybe KinesisStreamsInput,
    -- | Name prefix to use when creating an in-application stream. Suppose that
    -- you specify a prefix \"MyInApplicationStream.\" Amazon Kinesis Analytics
    -- then creates one or more (as per the @InputParallelism@ count you
    -- specified) in-application streams with names
    -- \"MyInApplicationStream_001,\" \"MyInApplicationStream_002,\" and so on.
    namePrefix :: Prelude.Text,
    -- | Describes the format of the data in the streaming source, and how each
    -- data element maps to corresponding columns in the in-application stream
    -- that is being created.
    --
    -- Also used to describe the format of the reference data source.
    inputSchema :: SourceSchema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Input' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputParallelism', 'input_inputParallelism' - Describes the number of in-application streams to create.
--
-- Data from your source is routed to these in-application input streams.
--
-- (see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
--
-- 'inputProcessingConfiguration', 'input_inputProcessingConfiguration' - The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- for the input. An input processor transforms records as they are
-- received from the stream, before the application\'s SQL code executes.
-- Currently, the only input processing configuration available is
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
--
-- 'kinesisFirehoseInput', 'input_kinesisFirehoseInput' - If the streaming source is an Amazon Kinesis Firehose delivery stream,
-- identifies the delivery stream\'s ARN and an IAM role that enables
-- Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
-- required.
--
-- 'kinesisStreamsInput', 'input_kinesisStreamsInput' - If the streaming source is an Amazon Kinesis stream, identifies the
-- stream\'s Amazon Resource Name (ARN) and an IAM role that enables Amazon
-- Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
-- required.
--
-- 'namePrefix', 'input_namePrefix' - Name prefix to use when creating an in-application stream. Suppose that
-- you specify a prefix \"MyInApplicationStream.\" Amazon Kinesis Analytics
-- then creates one or more (as per the @InputParallelism@ count you
-- specified) in-application streams with names
-- \"MyInApplicationStream_001,\" \"MyInApplicationStream_002,\" and so on.
--
-- 'inputSchema', 'input_inputSchema' - Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- Also used to describe the format of the reference data source.
newInput ::
  -- | 'namePrefix'
  Prelude.Text ->
  -- | 'inputSchema'
  SourceSchema ->
  Input
newInput pNamePrefix_ pInputSchema_ =
  Input'
    { inputParallelism = Prelude.Nothing,
      inputProcessingConfiguration = Prelude.Nothing,
      kinesisFirehoseInput = Prelude.Nothing,
      kinesisStreamsInput = Prelude.Nothing,
      namePrefix = pNamePrefix_,
      inputSchema = pInputSchema_
    }

-- | Describes the number of in-application streams to create.
--
-- Data from your source is routed to these in-application input streams.
--
-- (see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input>.
input_inputParallelism :: Lens.Lens' Input (Prelude.Maybe InputParallelism)
input_inputParallelism = Lens.lens (\Input' {inputParallelism} -> inputParallelism) (\s@Input' {} a -> s {inputParallelism = a} :: Input)

-- | The
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration>
-- for the input. An input processor transforms records as they are
-- received from the stream, before the application\'s SQL code executes.
-- Currently, the only input processing configuration available is
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor>.
input_inputProcessingConfiguration :: Lens.Lens' Input (Prelude.Maybe InputProcessingConfiguration)
input_inputProcessingConfiguration = Lens.lens (\Input' {inputProcessingConfiguration} -> inputProcessingConfiguration) (\s@Input' {} a -> s {inputProcessingConfiguration = a} :: Input)

-- | If the streaming source is an Amazon Kinesis Firehose delivery stream,
-- identifies the delivery stream\'s ARN and an IAM role that enables
-- Amazon Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
-- required.
input_kinesisFirehoseInput :: Lens.Lens' Input (Prelude.Maybe KinesisFirehoseInput)
input_kinesisFirehoseInput = Lens.lens (\Input' {kinesisFirehoseInput} -> kinesisFirehoseInput) (\s@Input' {} a -> s {kinesisFirehoseInput = a} :: Input)

-- | If the streaming source is an Amazon Kinesis stream, identifies the
-- stream\'s Amazon Resource Name (ARN) and an IAM role that enables Amazon
-- Kinesis Analytics to access the stream on your behalf.
--
-- Note: Either @KinesisStreamsInput@ or @KinesisFirehoseInput@ is
-- required.
input_kinesisStreamsInput :: Lens.Lens' Input (Prelude.Maybe KinesisStreamsInput)
input_kinesisStreamsInput = Lens.lens (\Input' {kinesisStreamsInput} -> kinesisStreamsInput) (\s@Input' {} a -> s {kinesisStreamsInput = a} :: Input)

-- | Name prefix to use when creating an in-application stream. Suppose that
-- you specify a prefix \"MyInApplicationStream.\" Amazon Kinesis Analytics
-- then creates one or more (as per the @InputParallelism@ count you
-- specified) in-application streams with names
-- \"MyInApplicationStream_001,\" \"MyInApplicationStream_002,\" and so on.
input_namePrefix :: Lens.Lens' Input Prelude.Text
input_namePrefix = Lens.lens (\Input' {namePrefix} -> namePrefix) (\s@Input' {} a -> s {namePrefix = a} :: Input)

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns in the in-application stream
-- that is being created.
--
-- Also used to describe the format of the reference data source.
input_inputSchema :: Lens.Lens' Input SourceSchema
input_inputSchema = Lens.lens (\Input' {inputSchema} -> inputSchema) (\s@Input' {} a -> s {inputSchema = a} :: Input)

instance Prelude.Hashable Input where
  hashWithSalt _salt Input' {..} =
    _salt
      `Prelude.hashWithSalt` inputParallelism
      `Prelude.hashWithSalt` inputProcessingConfiguration
      `Prelude.hashWithSalt` kinesisFirehoseInput
      `Prelude.hashWithSalt` kinesisStreamsInput
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` inputSchema

instance Prelude.NFData Input where
  rnf Input' {..} =
    Prelude.rnf inputParallelism
      `Prelude.seq` Prelude.rnf inputProcessingConfiguration
      `Prelude.seq` Prelude.rnf kinesisFirehoseInput
      `Prelude.seq` Prelude.rnf kinesisStreamsInput
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf inputSchema

instance Data.ToJSON Input where
  toJSON Input' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputParallelism" Data..=)
              Prelude.<$> inputParallelism,
            ("InputProcessingConfiguration" Data..=)
              Prelude.<$> inputProcessingConfiguration,
            ("KinesisFirehoseInput" Data..=)
              Prelude.<$> kinesisFirehoseInput,
            ("KinesisStreamsInput" Data..=)
              Prelude.<$> kinesisStreamsInput,
            Prelude.Just ("NamePrefix" Data..= namePrefix),
            Prelude.Just ("InputSchema" Data..= inputSchema)
          ]
      )
