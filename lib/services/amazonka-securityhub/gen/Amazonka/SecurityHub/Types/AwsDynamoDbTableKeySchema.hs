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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableKeySchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A component of the key schema for the DynamoDB table, a global secondary
-- index, or a local secondary index.
--
-- /See:/ 'newAwsDynamoDbTableKeySchema' smart constructor.
data AwsDynamoDbTableKeySchema = AwsDynamoDbTableKeySchema'
  { -- | The type of key used for the key schema attribute. Valid values are
    -- @HASH@ or @RANGE@.
    keyType :: Prelude.Maybe Prelude.Text,
    -- | The name of the key schema attribute.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableKeySchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyType', 'awsDynamoDbTableKeySchema_keyType' - The type of key used for the key schema attribute. Valid values are
-- @HASH@ or @RANGE@.
--
-- 'attributeName', 'awsDynamoDbTableKeySchema_attributeName' - The name of the key schema attribute.
newAwsDynamoDbTableKeySchema ::
  AwsDynamoDbTableKeySchema
newAwsDynamoDbTableKeySchema =
  AwsDynamoDbTableKeySchema'
    { keyType =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The type of key used for the key schema attribute. Valid values are
-- @HASH@ or @RANGE@.
awsDynamoDbTableKeySchema_keyType :: Lens.Lens' AwsDynamoDbTableKeySchema (Prelude.Maybe Prelude.Text)
awsDynamoDbTableKeySchema_keyType = Lens.lens (\AwsDynamoDbTableKeySchema' {keyType} -> keyType) (\s@AwsDynamoDbTableKeySchema' {} a -> s {keyType = a} :: AwsDynamoDbTableKeySchema)

-- | The name of the key schema attribute.
awsDynamoDbTableKeySchema_attributeName :: Lens.Lens' AwsDynamoDbTableKeySchema (Prelude.Maybe Prelude.Text)
awsDynamoDbTableKeySchema_attributeName = Lens.lens (\AwsDynamoDbTableKeySchema' {attributeName} -> attributeName) (\s@AwsDynamoDbTableKeySchema' {} a -> s {attributeName = a} :: AwsDynamoDbTableKeySchema)

instance Data.FromJSON AwsDynamoDbTableKeySchema where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableKeySchema"
      ( \x ->
          AwsDynamoDbTableKeySchema'
            Prelude.<$> (x Data..:? "KeyType")
            Prelude.<*> (x Data..:? "AttributeName")
      )

instance Prelude.Hashable AwsDynamoDbTableKeySchema where
  hashWithSalt _salt AwsDynamoDbTableKeySchema' {..} =
    _salt `Prelude.hashWithSalt` keyType
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData AwsDynamoDbTableKeySchema where
  rnf AwsDynamoDbTableKeySchema' {..} =
    Prelude.rnf keyType
      `Prelude.seq` Prelude.rnf attributeName

instance Data.ToJSON AwsDynamoDbTableKeySchema where
  toJSON AwsDynamoDbTableKeySchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyType" Data..=) Prelude.<$> keyType,
            ("AttributeName" Data..=) Prelude.<$> attributeName
          ]
      )
