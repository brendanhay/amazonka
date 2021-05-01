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
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputUpdate where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
import Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes updates to the output configuration identified by the
-- @OutputId@.
--
-- /See:/ 'newOutputUpdate' smart constructor.
data OutputUpdate = OutputUpdate'
  { -- | Describes an Amazon Kinesis Firehose delivery stream as the destination
    -- for the output.
    kinesisFirehoseOutputUpdate :: Prelude.Maybe KinesisFirehoseOutputUpdate,
    -- | Describes the data format when records are written to the destination.
    -- For more information, see
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
    destinationSchemaUpdate :: Prelude.Maybe DestinationSchema,
    -- | Describes an Amazon Kinesis stream as the destination for the output.
    kinesisStreamsOutputUpdate :: Prelude.Maybe KinesisStreamsOutputUpdate,
    -- | If you want to specify a different in-application stream for this output
    -- configuration, use this field to specify the new in-application stream
    -- name.
    nameUpdate :: Prelude.Maybe Prelude.Text,
    -- | Describes an AWS Lambda function as the destination for the output.
    lambdaOutputUpdate :: Prelude.Maybe LambdaOutputUpdate,
    -- | Identifies the specific output configuration that you want to update.
    outputId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  OutputUpdate
newOutputUpdate pOutputId_ =
  OutputUpdate'
    { kinesisFirehoseOutputUpdate =
        Prelude.Nothing,
      destinationSchemaUpdate = Prelude.Nothing,
      kinesisStreamsOutputUpdate = Prelude.Nothing,
      nameUpdate = Prelude.Nothing,
      lambdaOutputUpdate = Prelude.Nothing,
      outputId = pOutputId_
    }

-- | Describes an Amazon Kinesis Firehose delivery stream as the destination
-- for the output.
outputUpdate_kinesisFirehoseOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe KinesisFirehoseOutputUpdate)
outputUpdate_kinesisFirehoseOutputUpdate = Lens.lens (\OutputUpdate' {kinesisFirehoseOutputUpdate} -> kinesisFirehoseOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisFirehoseOutputUpdate = a} :: OutputUpdate)

-- | Describes the data format when records are written to the destination.
-- For more information, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Configuring Application Output>.
outputUpdate_destinationSchemaUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe DestinationSchema)
outputUpdate_destinationSchemaUpdate = Lens.lens (\OutputUpdate' {destinationSchemaUpdate} -> destinationSchemaUpdate) (\s@OutputUpdate' {} a -> s {destinationSchemaUpdate = a} :: OutputUpdate)

-- | Describes an Amazon Kinesis stream as the destination for the output.
outputUpdate_kinesisStreamsOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe KinesisStreamsOutputUpdate)
outputUpdate_kinesisStreamsOutputUpdate = Lens.lens (\OutputUpdate' {kinesisStreamsOutputUpdate} -> kinesisStreamsOutputUpdate) (\s@OutputUpdate' {} a -> s {kinesisStreamsOutputUpdate = a} :: OutputUpdate)

-- | If you want to specify a different in-application stream for this output
-- configuration, use this field to specify the new in-application stream
-- name.
outputUpdate_nameUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe Prelude.Text)
outputUpdate_nameUpdate = Lens.lens (\OutputUpdate' {nameUpdate} -> nameUpdate) (\s@OutputUpdate' {} a -> s {nameUpdate = a} :: OutputUpdate)

-- | Describes an AWS Lambda function as the destination for the output.
outputUpdate_lambdaOutputUpdate :: Lens.Lens' OutputUpdate (Prelude.Maybe LambdaOutputUpdate)
outputUpdate_lambdaOutputUpdate = Lens.lens (\OutputUpdate' {lambdaOutputUpdate} -> lambdaOutputUpdate) (\s@OutputUpdate' {} a -> s {lambdaOutputUpdate = a} :: OutputUpdate)

-- | Identifies the specific output configuration that you want to update.
outputUpdate_outputId :: Lens.Lens' OutputUpdate Prelude.Text
outputUpdate_outputId = Lens.lens (\OutputUpdate' {outputId} -> outputId) (\s@OutputUpdate' {} a -> s {outputId = a} :: OutputUpdate)

instance Prelude.Hashable OutputUpdate

instance Prelude.NFData OutputUpdate

instance Prelude.ToJSON OutputUpdate where
  toJSON OutputUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KinesisFirehoseOutputUpdate" Prelude..=)
              Prelude.<$> kinesisFirehoseOutputUpdate,
            ("DestinationSchemaUpdate" Prelude..=)
              Prelude.<$> destinationSchemaUpdate,
            ("KinesisStreamsOutputUpdate" Prelude..=)
              Prelude.<$> kinesisStreamsOutputUpdate,
            ("NameUpdate" Prelude..=) Prelude.<$> nameUpdate,
            ("LambdaOutputUpdate" Prelude..=)
              Prelude.<$> lambdaOutputUpdate,
            Prelude.Just ("OutputId" Prelude..= outputId)
          ]
      )
