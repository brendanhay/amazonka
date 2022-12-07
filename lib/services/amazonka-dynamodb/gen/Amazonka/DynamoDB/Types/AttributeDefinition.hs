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
-- Module      : Amazonka.DynamoDB.Types.AttributeDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.AttributeDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.ScalarAttributeType
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Represents an attribute for describing the key schema for the table and
-- indexes.
--
-- /See:/ 'newAttributeDefinition' smart constructor.
data AttributeDefinition = AttributeDefinition'
  { -- | A name for the attribute.
    attributeName :: Prelude.Text,
    -- | The data type for the attribute, where:
    --
    -- -   @S@ - the attribute is of type String
    --
    -- -   @N@ - the attribute is of type Number
    --
    -- -   @B@ - the attribute is of type Binary
    attributeType :: ScalarAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'attributeDefinition_attributeName' - A name for the attribute.
--
-- 'attributeType', 'attributeDefinition_attributeType' - The data type for the attribute, where:
--
-- -   @S@ - the attribute is of type String
--
-- -   @N@ - the attribute is of type Number
--
-- -   @B@ - the attribute is of type Binary
newAttributeDefinition ::
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'attributeType'
  ScalarAttributeType ->
  AttributeDefinition
newAttributeDefinition
  pAttributeName_
  pAttributeType_ =
    AttributeDefinition'
      { attributeName =
          pAttributeName_,
        attributeType = pAttributeType_
      }

-- | A name for the attribute.
attributeDefinition_attributeName :: Lens.Lens' AttributeDefinition Prelude.Text
attributeDefinition_attributeName = Lens.lens (\AttributeDefinition' {attributeName} -> attributeName) (\s@AttributeDefinition' {} a -> s {attributeName = a} :: AttributeDefinition)

-- | The data type for the attribute, where:
--
-- -   @S@ - the attribute is of type String
--
-- -   @N@ - the attribute is of type Number
--
-- -   @B@ - the attribute is of type Binary
attributeDefinition_attributeType :: Lens.Lens' AttributeDefinition ScalarAttributeType
attributeDefinition_attributeType = Lens.lens (\AttributeDefinition' {attributeType} -> attributeType) (\s@AttributeDefinition' {} a -> s {attributeType = a} :: AttributeDefinition)

instance Data.FromJSON AttributeDefinition where
  parseJSON =
    Data.withObject
      "AttributeDefinition"
      ( \x ->
          AttributeDefinition'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> (x Data..: "AttributeType")
      )

instance Prelude.Hashable AttributeDefinition where
  hashWithSalt _salt AttributeDefinition' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` attributeType

instance Prelude.NFData AttributeDefinition where
  rnf AttributeDefinition' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf attributeType

instance Data.ToJSON AttributeDefinition where
  toJSON AttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just
              ("AttributeType" Data..= attributeType)
          ]
      )
