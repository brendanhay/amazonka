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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.Output
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.DestinationSchema
import Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutput
import Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutput
import Amazonka.KinesisAnalyticsV2.Types.LambdaOutput
import qualified Amazonka.Prelude as Prelude

-- | Describes a SQL-based Kinesis Data Analytics application\'s output
-- configuration, in which you identify an in-application stream and a
-- destination where you want the in-application stream data to be written.
-- The destination can be a Kinesis data stream or a Kinesis Data Firehose
-- delivery stream.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | Identifies a Kinesis Data Firehose delivery stream as the destination.
    kinesisFirehoseOutput :: Prelude.Maybe KinesisFirehoseOutput,
    -- | Identifies an Amazon Lambda function as the destination.
    lambdaOutput :: Prelude.Maybe LambdaOutput,
    -- | Identifies a Kinesis data stream as the destination.
    kinesisStreamsOutput :: Prelude.Maybe KinesisStreamsOutput,
    -- | The name of the in-application stream.
    name :: Prelude.Text,
    -- | Describes the data format when records are written to the destination.
    destinationSchema :: DestinationSchema
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kinesisFirehoseOutput', 'output_kinesisFirehoseOutput' - Identifies a Kinesis Data Firehose delivery stream as the destination.
--
-- 'lambdaOutput', 'output_lambdaOutput' - Identifies an Amazon Lambda function as the destination.
--
-- 'kinesisStreamsOutput', 'output_kinesisStreamsOutput' - Identifies a Kinesis data stream as the destination.
--
-- 'name', 'output_name' - The name of the in-application stream.
--
-- 'destinationSchema', 'output_destinationSchema' - Describes the data format when records are written to the destination.
newOutput ::
  -- | 'name'
  Prelude.Text ->
  -- | 'destinationSchema'
  DestinationSchema ->
  Output
newOutput pName_ pDestinationSchema_ =
  Output'
    { kinesisFirehoseOutput = Prelude.Nothing,
      lambdaOutput = Prelude.Nothing,
      kinesisStreamsOutput = Prelude.Nothing,
      name = pName_,
      destinationSchema = pDestinationSchema_
    }

-- | Identifies a Kinesis Data Firehose delivery stream as the destination.
output_kinesisFirehoseOutput :: Lens.Lens' Output (Prelude.Maybe KinesisFirehoseOutput)
output_kinesisFirehoseOutput = Lens.lens (\Output' {kinesisFirehoseOutput} -> kinesisFirehoseOutput) (\s@Output' {} a -> s {kinesisFirehoseOutput = a} :: Output)

-- | Identifies an Amazon Lambda function as the destination.
output_lambdaOutput :: Lens.Lens' Output (Prelude.Maybe LambdaOutput)
output_lambdaOutput = Lens.lens (\Output' {lambdaOutput} -> lambdaOutput) (\s@Output' {} a -> s {lambdaOutput = a} :: Output)

-- | Identifies a Kinesis data stream as the destination.
output_kinesisStreamsOutput :: Lens.Lens' Output (Prelude.Maybe KinesisStreamsOutput)
output_kinesisStreamsOutput = Lens.lens (\Output' {kinesisStreamsOutput} -> kinesisStreamsOutput) (\s@Output' {} a -> s {kinesisStreamsOutput = a} :: Output)

-- | The name of the in-application stream.
output_name :: Lens.Lens' Output Prelude.Text
output_name = Lens.lens (\Output' {name} -> name) (\s@Output' {} a -> s {name = a} :: Output)

-- | Describes the data format when records are written to the destination.
output_destinationSchema :: Lens.Lens' Output DestinationSchema
output_destinationSchema = Lens.lens (\Output' {destinationSchema} -> destinationSchema) (\s@Output' {} a -> s {destinationSchema = a} :: Output)

instance Prelude.Hashable Output where
  hashWithSalt _salt Output' {..} =
    _salt `Prelude.hashWithSalt` kinesisFirehoseOutput
      `Prelude.hashWithSalt` lambdaOutput
      `Prelude.hashWithSalt` kinesisStreamsOutput
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` destinationSchema

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf kinesisFirehoseOutput
      `Prelude.seq` Prelude.rnf lambdaOutput
      `Prelude.seq` Prelude.rnf kinesisStreamsOutput
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf destinationSchema

instance Data.ToJSON Output where
  toJSON Output' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KinesisFirehoseOutput" Data..=)
              Prelude.<$> kinesisFirehoseOutput,
            ("LambdaOutput" Data..=) Prelude.<$> lambdaOutput,
            ("KinesisStreamsOutput" Data..=)
              Prelude.<$> kinesisStreamsOutput,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DestinationSchema" Data..= destinationSchema)
          ]
      )
