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
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import qualified Network.AWS.Lens as Lens

-- | Describes updates to the output configuration identified by the
-- @OutputId@.
--
-- /See:/ 'newOutputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { -- | Describes an Amazon Kinesis Firehose delivery stream as the destination
    -- for the output.
    kinesisFirehoseOutputUpdate :: Core.Maybe KinesisFirehoseOutputUpdate,
    -- | Describes the data format when records are written to the destination.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    destinationSchemaUpdate :: Core.Maybe DestinationSchema,
    -- | Describes an Amazon Kinesis stream as the destination for the output.
    kinesisStreamsOutputUpdate :: Core.Maybe KinesisStreamsOutputUpdate,
    -- | If you want to specify a different in-application stream for this output
    -- configuration, use this field to specify the new in-application stream
    -- name.
    nameUpdate :: Core.Maybe Core.Text,
    -- | Describes an AWS Lambda function as the destination for the output.
    lambdaOutputUpdate :: Core.Maybe LambdaOutputUpdate,
    -- | Identifies the specific output configuration that you want to update.
    outputId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisFirehoseOutputUpdate', 'outputUpdate_kinesisFirehoseOutputUpdate' - Describes an Amazon Kinesis Firehose delivery stream as the destination
-- for the output.
--
-- 'destinationSchemaUpdate', 'outputUpdate_destinationSchemaUpdate' - Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
--
-- 'kinesisStreamsOutputUpdate', 'outputUpdate_kinesisStreamsOutputUpdate' - Describes an Amazon Kinesis stream as the destination for the output.
--
-- 'nameUpdate', 'outputUpdate_nameUpdate' - If you want to specify a different in-application stream for this output
-- configuration, use this field to specify the new in-application stream
-- name.
--
-- 'lambdaOutputUpdate', 'outputUpdate_lambdaOutputUpdate' - Describes an AWS Lambda function as the destination for the output.
--
-- 'outputId', 'outputUpdate_outputId' - Identifies the specific output configuration that you want to update.
newOutputUpdate ::
  -- | 'outputId'
  Core.Text ->
  OutputUpdate
newOutputUpdate pOutputId_ =
  OutputUpdate'
    { kinesisFirehoseOutputUpdate =
        Core.Nothing,
      destinationSchemaUpdate = Core.Nothing,
      kinesisStreamsOutputUpdate = Core.Nothing,
      nameUpdate = Core.Nothing,
      lambdaOutputUpdate = Core.Nothing,
      outputId = pOutputId_
    }

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination
-- for the output.
outputUpdate_kinesisFirehoseOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe KinesisFirehoseOutputUpdate)
outputUpdate_kinesisFirehoseOutputUpdate = Lens.lens (\OutputUpdate' {kinesisFirehoseOutputUpdate} -> kinesisFirehoseOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisFirehoseOutputUpdate = a} :: OutputUpdate)

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
outputUpdate_destinationSchemaUpdate :: Lens.Lens' OutputUpdate (Core.Maybe DestinationSchema)
outputUpdate_destinationSchemaUpdate = Lens.lens (\OutputUpdate' {destinationSchemaUpdate} -> destinationSchemaUpdate) (\s@OutputUpdate' {} a -> s {destinationSchemaUpdate = a} :: OutputUpdate)

-- | Describes an Amazon Kinesis stream as the destination for the output.
outputUpdate_kinesisStreamsOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe KinesisStreamsOutputUpdate)
outputUpdate_kinesisStreamsOutputUpdate = Lens.lens (\OutputUpdate' {kinesisStreamsOutputUpdate} -> kinesisStreamsOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisStreamsOutputUpdate = a} :: OutputUpdate)

-- | If you want to specify a different in-application stream for this output
-- configuration, use this field to specify the new in-application stream
-- name.
outputUpdate_nameUpdate :: Lens.Lens' OutputUpdate (Core.Maybe Core.Text)
outputUpdate_nameUpdate = Lens.lens (\OutputUpdate' {nameUpdate} -> nameUpdate) (\s@OutputUpdate' {} a -> s {nameUpdate = a} :: OutputUpdate)

-- | Describes an AWS Lambda function as the destination for the output.
outputUpdate_lambdaOutputUpdate :: Lens.Lens' OutputUpdate (Core.Maybe LambdaOutputUpdate)
outputUpdate_lambdaOutputUpdate = Lens.lens (\OutputUpdate' {lambdaOutputUpdate} -> lambdaOutputUpdate) (\s@OutputUpdate' {} a -> s {lambdaOutputUpdate = a} :: OutputUpdate)

-- | Identifies the specific output configuration that you want to update.
outputUpdate_outputId :: Lens.Lens' OutputUpdate Core.Text
outputUpdate_outputId = Lens.lens (\OutputUpdate' {outputId} -> outputId) (\s@OutputUpdate' {} a -> s {outputId = a} :: OutputUpdate)

instance Core.Hashable OutputUpdate

instance Core.NFData OutputUpdate

instance Core.ToJSON OutputUpdate where
  toJSON OutputUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KinesisFirehoseOutputUpdate" Core..=)
              Core.<$> kinesisFirehoseOutputUpdate,
            ("DestinationSchemaUpdate" Core..=)
              Core.<$> destinationSchemaUpdate,
            ("KinesisStreamsOutputUpdate" Core..=)
              Core.<$> kinesisStreamsOutputUpdate,
            ("NameUpdate" Core..=) Core.<$> nameUpdate,
            ("LambdaOutputUpdate" Core..=)
              Core.<$> lambdaOutputUpdate,
            Core.Just ("OutputId" Core..= outputId)
          ]
      )
