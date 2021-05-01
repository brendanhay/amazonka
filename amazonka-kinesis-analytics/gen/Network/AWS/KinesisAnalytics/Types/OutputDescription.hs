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
-- Module      : Network.AWS.KinesisAnalytics.Types.OutputDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.OutputDescription where

import Network.AWS.KinesisAnalytics.Types.DestinationSchema
import Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
import Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
import Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the application output configuration, which includes the
-- in-application stream name and the destination where the stream data is
-- written. The destination can be an Amazon Kinesis stream or an Amazon
-- Kinesis Firehose delivery stream.
--
-- /See:/ 'newOutputDescription' smart constructor.
data OutputDescription = OutputDescription'
  { -- | Describes Amazon Kinesis stream configured as the destination where
    -- output is written.
    kinesisStreamsOutputDescription :: Prelude.Maybe KinesisStreamsOutputDescription,
    -- | Describes the Amazon Kinesis Firehose delivery stream configured as the
    -- destination where output is written.
    kinesisFirehoseOutputDescription :: Prelude.Maybe KinesisFirehoseOutputDescription,
    -- | Data format used for writing data to the destination.
    destinationSchema :: Prelude.Maybe DestinationSchema,
    -- | A unique identifier for the output configuration.
    outputId :: Prelude.Maybe Prelude.Text,
    -- | Name of the in-application stream configured as output.
    name :: Prelude.Maybe Prelude.Text,
    -- | Describes the AWS Lambda function configured as the destination where
    -- output is written.
    lambdaOutputDescription :: Prelude.Maybe LambdaOutputDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisStreamsOutputDescription', 'outputDescription_kinesisStreamsOutputDescription' - Describes Amazon Kinesis stream configured as the destination where
-- output is written.
--
-- 'kinesisFirehoseOutputDescription', 'outputDescription_kinesisFirehoseOutputDescription' - Describes the Amazon Kinesis Firehose delivery stream configured as the
-- destination where output is written.
--
-- 'destinationSchema', 'outputDescription_destinationSchema' - Data format used for writing data to the destination.
--
-- 'outputId', 'outputDescription_outputId' - A unique identifier for the output configuration.
--
-- 'name', 'outputDescription_name' - Name of the in-application stream configured as output.
--
-- 'lambdaOutputDescription', 'outputDescription_lambdaOutputDescription' - Describes the AWS Lambda function configured as the destination where
-- output is written.
newOutputDescription ::
  OutputDescription
newOutputDescription =
  OutputDescription'
    { kinesisStreamsOutputDescription =
        Prelude.Nothing,
      kinesisFirehoseOutputDescription = Prelude.Nothing,
      destinationSchema = Prelude.Nothing,
      outputId = Prelude.Nothing,
      name = Prelude.Nothing,
      lambdaOutputDescription = Prelude.Nothing
    }

-- | Describes Amazon Kinesis stream configured as the destination where
-- output is written.
outputDescription_kinesisStreamsOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe KinesisStreamsOutputDescription)
outputDescription_kinesisStreamsOutputDescription = Lens.lens (\OutputDescription' {kinesisStreamsOutputDescription} -> kinesisStreamsOutputDescription) (\s@OutputDescription' {} a -> s {kinesisStreamsOutputDescription = a} :: OutputDescription)

-- | Describes the Amazon Kinesis Firehose delivery stream configured as the
-- destination where output is written.
outputDescription_kinesisFirehoseOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe KinesisFirehoseOutputDescription)
outputDescription_kinesisFirehoseOutputDescription = Lens.lens (\OutputDescription' {kinesisFirehoseOutputDescription} -> kinesisFirehoseOutputDescription) (\s@OutputDescription' {} a -> s {kinesisFirehoseOutputDescription = a} :: OutputDescription)

-- | Data format used for writing data to the destination.
outputDescription_destinationSchema :: Lens.Lens' OutputDescription (Prelude.Maybe DestinationSchema)
outputDescription_destinationSchema = Lens.lens (\OutputDescription' {destinationSchema} -> destinationSchema) (\s@OutputDescription' {} a -> s {destinationSchema = a} :: OutputDescription)

-- | A unique identifier for the output configuration.
outputDescription_outputId :: Lens.Lens' OutputDescription (Prelude.Maybe Prelude.Text)
outputDescription_outputId = Lens.lens (\OutputDescription' {outputId} -> outputId) (\s@OutputDescription' {} a -> s {outputId = a} :: OutputDescription)

-- | Name of the in-application stream configured as output.
outputDescription_name :: Lens.Lens' OutputDescription (Prelude.Maybe Prelude.Text)
outputDescription_name = Lens.lens (\OutputDescription' {name} -> name) (\s@OutputDescription' {} a -> s {name = a} :: OutputDescription)

-- | Describes the AWS Lambda function configured as the destination where
-- output is written.
outputDescription_lambdaOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe LambdaOutputDescription)
outputDescription_lambdaOutputDescription = Lens.lens (\OutputDescription' {lambdaOutputDescription} -> lambdaOutputDescription) (\s@OutputDescription' {} a -> s {lambdaOutputDescription = a} :: OutputDescription)

instance Prelude.FromJSON OutputDescription where
  parseJSON =
    Prelude.withObject
      "OutputDescription"
      ( \x ->
          OutputDescription'
            Prelude.<$> (x Prelude..:? "KinesisStreamsOutputDescription")
            Prelude.<*> (x Prelude..:? "KinesisFirehoseOutputDescription")
            Prelude.<*> (x Prelude..:? "DestinationSchema")
            Prelude.<*> (x Prelude..:? "OutputId")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "LambdaOutputDescription")
      )

instance Prelude.Hashable OutputDescription

instance Prelude.NFData OutputDescription
