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
-- Module      : Network.AWS.SecurityHub.Types.AwsDynamoDbTableKeySchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsDynamoDbTableKeySchema where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A component of the key schema for the DynamoDB table, a global secondary
-- index, or a local secondary index.
--
-- /See:/ 'newAwsDynamoDbTableKeySchema' smart constructor.
data AwsDynamoDbTableKeySchema = AwsDynamoDbTableKeySchema'
  { -- | The type of key used for the key schema attribute.
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
-- 'keyType', 'awsDynamoDbTableKeySchema_keyType' - The type of key used for the key schema attribute.
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

-- | The type of key used for the key schema attribute.
awsDynamoDbTableKeySchema_keyType :: Lens.Lens' AwsDynamoDbTableKeySchema (Prelude.Maybe Prelude.Text)
awsDynamoDbTableKeySchema_keyType = Lens.lens (\AwsDynamoDbTableKeySchema' {keyType} -> keyType) (\s@AwsDynamoDbTableKeySchema' {} a -> s {keyType = a} :: AwsDynamoDbTableKeySchema)

-- | The name of the key schema attribute.
awsDynamoDbTableKeySchema_attributeName :: Lens.Lens' AwsDynamoDbTableKeySchema (Prelude.Maybe Prelude.Text)
awsDynamoDbTableKeySchema_attributeName = Lens.lens (\AwsDynamoDbTableKeySchema' {attributeName} -> attributeName) (\s@AwsDynamoDbTableKeySchema' {} a -> s {attributeName = a} :: AwsDynamoDbTableKeySchema)

instance Core.FromJSON AwsDynamoDbTableKeySchema where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableKeySchema"
      ( \x ->
          AwsDynamoDbTableKeySchema'
            Prelude.<$> (x Core..:? "KeyType")
            Prelude.<*> (x Core..:? "AttributeName")
      )

instance Prelude.Hashable AwsDynamoDbTableKeySchema

instance Prelude.NFData AwsDynamoDbTableKeySchema

instance Core.ToJSON AwsDynamoDbTableKeySchema where
  toJSON AwsDynamoDbTableKeySchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeyType" Core..=) Prelude.<$> keyType,
            ("AttributeName" Core..=) Prelude.<$> attributeName
          ]
      )
