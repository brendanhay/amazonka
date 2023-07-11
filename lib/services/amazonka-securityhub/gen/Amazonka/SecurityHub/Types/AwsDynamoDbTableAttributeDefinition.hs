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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableAttributeDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a definition of an attribute for the table.
--
-- /See:/ 'newAwsDynamoDbTableAttributeDefinition' smart constructor.
data AwsDynamoDbTableAttributeDefinition = AwsDynamoDbTableAttributeDefinition'
  { -- | The name of the attribute.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The type of the attribute.
    attributeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'awsDynamoDbTableAttributeDefinition_attributeName' - The name of the attribute.
--
-- 'attributeType', 'awsDynamoDbTableAttributeDefinition_attributeType' - The type of the attribute.
newAwsDynamoDbTableAttributeDefinition ::
  AwsDynamoDbTableAttributeDefinition
newAwsDynamoDbTableAttributeDefinition =
  AwsDynamoDbTableAttributeDefinition'
    { attributeName =
        Prelude.Nothing,
      attributeType = Prelude.Nothing
    }

-- | The name of the attribute.
awsDynamoDbTableAttributeDefinition_attributeName :: Lens.Lens' AwsDynamoDbTableAttributeDefinition (Prelude.Maybe Prelude.Text)
awsDynamoDbTableAttributeDefinition_attributeName = Lens.lens (\AwsDynamoDbTableAttributeDefinition' {attributeName} -> attributeName) (\s@AwsDynamoDbTableAttributeDefinition' {} a -> s {attributeName = a} :: AwsDynamoDbTableAttributeDefinition)

-- | The type of the attribute.
awsDynamoDbTableAttributeDefinition_attributeType :: Lens.Lens' AwsDynamoDbTableAttributeDefinition (Prelude.Maybe Prelude.Text)
awsDynamoDbTableAttributeDefinition_attributeType = Lens.lens (\AwsDynamoDbTableAttributeDefinition' {attributeType} -> attributeType) (\s@AwsDynamoDbTableAttributeDefinition' {} a -> s {attributeType = a} :: AwsDynamoDbTableAttributeDefinition)

instance
  Data.FromJSON
    AwsDynamoDbTableAttributeDefinition
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableAttributeDefinition"
      ( \x ->
          AwsDynamoDbTableAttributeDefinition'
            Prelude.<$> (x Data..:? "AttributeName")
            Prelude.<*> (x Data..:? "AttributeType")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableAttributeDefinition
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableAttributeDefinition' {..} =
      _salt
        `Prelude.hashWithSalt` attributeName
        `Prelude.hashWithSalt` attributeType

instance
  Prelude.NFData
    AwsDynamoDbTableAttributeDefinition
  where
  rnf AwsDynamoDbTableAttributeDefinition' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf attributeType

instance
  Data.ToJSON
    AwsDynamoDbTableAttributeDefinition
  where
  toJSON AwsDynamoDbTableAttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeName" Data..=) Prelude.<$> attributeName,
            ("AttributeType" Data..=) Prelude.<$> attributeType
          ]
      )
