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
-- Module      : Network.AWS.KinesisAnalytics.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.Output where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
import Network.AWS.KinesisAnalytics.Types.LambdaOutput
import qualified Network.AWS.Lens as Lens

-- | Describes application output configuration in which you identify an
-- in-application stream and a destination where you want the
-- in-application stream data to be written. The destination can be an
-- Amazon Kinesis stream or an Amazon Kinesis Firehose delivery stream.
--
-- For limits on how many destinations an application can write and other
-- limitations, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits>.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | Identifies an AWS Lambda function as the destination.
    lambdaOutput :: Core.Maybe LambdaOutput,
    -- | Identifies an Amazon Kinesis Firehose delivery stream as the
    -- destination.
    kinesisFirehoseOutput :: Core.Maybe KinesisFirehoseOutput,
    -- | Identifies an Amazon Kinesis stream as the destination.
    kinesisStreamsOutput :: Core.Maybe KinesisStreamsOutput,
    -- | Name of the in-application stream.
    name :: Core.Text,
    -- | Describes the data format when records are written to the destination.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    destinationSchema :: DestinationSchema
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lambdaOutput', 'output_lambdaOutput' - Identifies an AWS Lambda function as the destination.
--
-- 'kinesisFirehoseOutput', 'output_kinesisFirehoseOutput' - Identifies an Amazon Kinesis Firehose delivery stream as the
-- destination.
--
-- 'kinesisStreamsOutput', 'output_kinesisStreamsOutput' - Identifies an Amazon Kinesis stream as the destination.
--
-- 'name', 'output_name' - Name of the in-application stream.
--
-- 'destinationSchema', 'output_destinationSchema' - Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
newOutput ::
  -- | 'name'
  Core.Text ->
  -- | 'destinationSchema'
  DestinationSchema ->
  Output
newOutput pName_ pDestinationSchema_ =
  Output'
    { lambdaOutput = Core.Nothing,
      kinesisFirehoseOutput = Core.Nothing,
      kinesisStreamsOutput = Core.Nothing,
      name = pName_,
      destinationSchema = pDestinationSchema_
    }

-- | Identifies an AWS Lambda function as the destination.
output_lambdaOutput :: Lens.Lens' Output (Core.Maybe LambdaOutput)
output_lambdaOutput = Lens.lens (\Output' {lambdaOutput} -> lambdaOutput) (\s@Output' {} a -> s {lambdaOutput = a} :: Output)

-- | Identifies an Amazon Kinesis Firehose delivery stream as the
-- destination.
output_kinesisFirehoseOutput :: Lens.Lens' Output (Core.Maybe KinesisFirehoseOutput)
output_kinesisFirehoseOutput = Lens.lens (\Output' {kinesisFirehoseOutput} -> kinesisFirehoseOutput) (\s@Output' {} a -> s {kinesisFirehoseOutput = a} :: Output)

-- | Identifies an Amazon Kinesis stream as the destination.
output_kinesisStreamsOutput :: Lens.Lens' Output (Core.Maybe KinesisStreamsOutput)
output_kinesisStreamsOutput = Lens.lens (\Output' {kinesisStreamsOutput} -> kinesisStreamsOutput) (\s@Output' {} a -> s {kinesisStreamsOutput = a} :: Output)

-- | Name of the in-application stream.
output_name :: Lens.Lens' Output Core.Text
output_name = Lens.lens (\Output' {name} -> name) (\s@Output' {} a -> s {name = a} :: Output)

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
output_destinationSchema :: Lens.Lens' Output DestinationSchema
output_destinationSchema = Lens.lens (\Output' {destinationSchema} -> destinationSchema) (\s@Output' {} a -> s {destinationSchema = a} :: Output)

instance Core.Hashable Output

instance Core.NFData Output

instance Core.ToJSON Output where
  toJSON Output' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LambdaOutput" Core..=) Core.<$> lambdaOutput,
            ("KinesisFirehoseOutput" Core..=)
              Core.<$> kinesisFirehoseOutput,
            ("KinesisStreamsOutput" Core..=)
              Core.<$> kinesisStreamsOutput,
            Core.Just ("Name" Core..= name),
            Core.Just
              ("DestinationSchema" Core..= destinationSchema)
          ]
      )
