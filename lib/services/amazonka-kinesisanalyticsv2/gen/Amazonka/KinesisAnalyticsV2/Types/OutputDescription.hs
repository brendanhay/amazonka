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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.OutputDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.OutputDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.DestinationSchema
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutputDescription
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputDescription
import Amazonka.KinesisAnalyticsV2.Types.LambdaOutputDescription
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, describes the
-- application output configuration, which includes the in-application
-- stream name and the destination where the stream data is written. The
-- destination can be a Kinesis data stream or a Kinesis Data Firehose
-- delivery stream.
--
-- /See:/ 'newOutputDescription' smart constructor.
data OutputDescription = OutputDescription'
  { -- | The data format used for writing data to the destination.
    destinationSchema :: Prelude.Maybe DestinationSchema,
    -- | Describes the Kinesis Data Firehose delivery stream that is configured
    -- as the destination where output is written.
    kinesisFirehoseOutputDescription :: Prelude.Maybe KinesisFirehoseOutputDescription,
    -- | Describes the Kinesis data stream that is configured as the destination
    -- where output is written.
    kinesisStreamsOutputDescription :: Prelude.Maybe KinesisStreamsOutputDescription,
    -- | Describes the Lambda function that is configured as the destination
    -- where output is written.
    lambdaOutputDescription :: Prelude.Maybe LambdaOutputDescription,
    -- | The name of the in-application stream that is configured as output.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the output configuration.
    outputId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationSchema', 'outputDescription_destinationSchema' - The data format used for writing data to the destination.
--
-- 'kinesisFirehoseOutputDescription', 'outputDescription_kinesisFirehoseOutputDescription' - Describes the Kinesis Data Firehose delivery stream that is configured
-- as the destination where output is written.
--
-- 'kinesisStreamsOutputDescription', 'outputDescription_kinesisStreamsOutputDescription' - Describes the Kinesis data stream that is configured as the destination
-- where output is written.
--
-- 'lambdaOutputDescription', 'outputDescription_lambdaOutputDescription' - Describes the Lambda function that is configured as the destination
-- where output is written.
--
-- 'name', 'outputDescription_name' - The name of the in-application stream that is configured as output.
--
-- 'outputId', 'outputDescription_outputId' - A unique identifier for the output configuration.
newOutputDescription ::
  OutputDescription
newOutputDescription =
  OutputDescription'
    { destinationSchema =
        Prelude.Nothing,
      kinesisFirehoseOutputDescription = Prelude.Nothing,
      kinesisStreamsOutputDescription = Prelude.Nothing,
      lambdaOutputDescription = Prelude.Nothing,
      name = Prelude.Nothing,
      outputId = Prelude.Nothing
    }

-- | The data format used for writing data to the destination.
outputDescription_destinationSchema :: Lens.Lens' OutputDescription (Prelude.Maybe DestinationSchema)
outputDescription_destinationSchema = Lens.lens (\OutputDescription' {destinationSchema} -> destinationSchema) (\s@OutputDescription' {} a -> s {destinationSchema = a} :: OutputDescription)

-- | Describes the Kinesis Data Firehose delivery stream that is configured
-- as the destination where output is written.
outputDescription_kinesisFirehoseOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe KinesisFirehoseOutputDescription)
outputDescription_kinesisFirehoseOutputDescription = Lens.lens (\OutputDescription' {kinesisFirehoseOutputDescription} -> kinesisFirehoseOutputDescription) (\s@OutputDescription' {} a -> s {kinesisFirehoseOutputDescription = a} :: OutputDescription)

-- | Describes the Kinesis data stream that is configured as the destination
-- where output is written.
outputDescription_kinesisStreamsOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe KinesisStreamsOutputDescription)
outputDescription_kinesisStreamsOutputDescription = Lens.lens (\OutputDescription' {kinesisStreamsOutputDescription} -> kinesisStreamsOutputDescription) (\s@OutputDescription' {} a -> s {kinesisStreamsOutputDescription = a} :: OutputDescription)

-- | Describes the Lambda function that is configured as the destination
-- where output is written.
outputDescription_lambdaOutputDescription :: Lens.Lens' OutputDescription (Prelude.Maybe LambdaOutputDescription)
outputDescription_lambdaOutputDescription = Lens.lens (\OutputDescription' {lambdaOutputDescription} -> lambdaOutputDescription) (\s@OutputDescription' {} a -> s {lambdaOutputDescription = a} :: OutputDescription)

-- | The name of the in-application stream that is configured as output.
outputDescription_name :: Lens.Lens' OutputDescription (Prelude.Maybe Prelude.Text)
outputDescription_name = Lens.lens (\OutputDescription' {name} -> name) (\s@OutputDescription' {} a -> s {name = a} :: OutputDescription)

-- | A unique identifier for the output configuration.
outputDescription_outputId :: Lens.Lens' OutputDescription (Prelude.Maybe Prelude.Text)
outputDescription_outputId = Lens.lens (\OutputDescription' {outputId} -> outputId) (\s@OutputDescription' {} a -> s {outputId = a} :: OutputDescription)

instance Data.FromJSON OutputDescription where
  parseJSON =
    Data.withObject
      "OutputDescription"
      ( \x ->
          OutputDescription'
            Prelude.<$> (x Data..:? "DestinationSchema")
            Prelude.<*> (x Data..:? "KinesisFirehoseOutputDescription")
            Prelude.<*> (x Data..:? "KinesisStreamsOutputDescription")
            Prelude.<*> (x Data..:? "LambdaOutputDescription")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutputId")
      )

instance Prelude.Hashable OutputDescription where
  hashWithSalt _salt OutputDescription' {..} =
    _salt
      `Prelude.hashWithSalt` destinationSchema
      `Prelude.hashWithSalt` kinesisFirehoseOutputDescription
      `Prelude.hashWithSalt` kinesisStreamsOutputDescription
      `Prelude.hashWithSalt` lambdaOutputDescription
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputId

instance Prelude.NFData OutputDescription where
  rnf OutputDescription' {..} =
    Prelude.rnf destinationSchema
      `Prelude.seq` Prelude.rnf kinesisFirehoseOutputDescription
      `Prelude.seq` Prelude.rnf kinesisStreamsOutputDescription
      `Prelude.seq` Prelude.rnf lambdaOutputDescription
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputId
