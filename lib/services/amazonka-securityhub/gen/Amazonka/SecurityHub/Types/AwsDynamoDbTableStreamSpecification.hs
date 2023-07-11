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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableStreamSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current DynamoDB Streams configuration for the table.
--
-- /See:/ 'newAwsDynamoDbTableStreamSpecification' smart constructor.
data AwsDynamoDbTableStreamSpecification = AwsDynamoDbTableStreamSpecification'
  { -- | Indicates whether DynamoDB Streams is enabled on the table.
    streamEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Determines the information that is written to the table.
    streamViewType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableStreamSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamEnabled', 'awsDynamoDbTableStreamSpecification_streamEnabled' - Indicates whether DynamoDB Streams is enabled on the table.
--
-- 'streamViewType', 'awsDynamoDbTableStreamSpecification_streamViewType' - Determines the information that is written to the table.
newAwsDynamoDbTableStreamSpecification ::
  AwsDynamoDbTableStreamSpecification
newAwsDynamoDbTableStreamSpecification =
  AwsDynamoDbTableStreamSpecification'
    { streamEnabled =
        Prelude.Nothing,
      streamViewType = Prelude.Nothing
    }

-- | Indicates whether DynamoDB Streams is enabled on the table.
awsDynamoDbTableStreamSpecification_streamEnabled :: Lens.Lens' AwsDynamoDbTableStreamSpecification (Prelude.Maybe Prelude.Bool)
awsDynamoDbTableStreamSpecification_streamEnabled = Lens.lens (\AwsDynamoDbTableStreamSpecification' {streamEnabled} -> streamEnabled) (\s@AwsDynamoDbTableStreamSpecification' {} a -> s {streamEnabled = a} :: AwsDynamoDbTableStreamSpecification)

-- | Determines the information that is written to the table.
awsDynamoDbTableStreamSpecification_streamViewType :: Lens.Lens' AwsDynamoDbTableStreamSpecification (Prelude.Maybe Prelude.Text)
awsDynamoDbTableStreamSpecification_streamViewType = Lens.lens (\AwsDynamoDbTableStreamSpecification' {streamViewType} -> streamViewType) (\s@AwsDynamoDbTableStreamSpecification' {} a -> s {streamViewType = a} :: AwsDynamoDbTableStreamSpecification)

instance
  Data.FromJSON
    AwsDynamoDbTableStreamSpecification
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableStreamSpecification"
      ( \x ->
          AwsDynamoDbTableStreamSpecification'
            Prelude.<$> (x Data..:? "StreamEnabled")
            Prelude.<*> (x Data..:? "StreamViewType")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableStreamSpecification
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableStreamSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` streamEnabled
        `Prelude.hashWithSalt` streamViewType

instance
  Prelude.NFData
    AwsDynamoDbTableStreamSpecification
  where
  rnf AwsDynamoDbTableStreamSpecification' {..} =
    Prelude.rnf streamEnabled
      `Prelude.seq` Prelude.rnf streamViewType

instance
  Data.ToJSON
    AwsDynamoDbTableStreamSpecification
  where
  toJSON AwsDynamoDbTableStreamSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StreamEnabled" Data..=) Prelude.<$> streamEnabled,
            ("StreamViewType" Data..=)
              Prelude.<$> streamViewType
          ]
      )
