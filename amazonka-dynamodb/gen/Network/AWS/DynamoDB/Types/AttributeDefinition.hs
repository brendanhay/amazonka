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
-- Module      : Network.AWS.DynamoDB.Types.AttributeDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.AttributeDefinition where

import Network.AWS.DynamoDB.Types.ScalarAttributeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AttributeDefinition where
  parseJSON =
    Prelude.withObject
      "AttributeDefinition"
      ( \x ->
          AttributeDefinition'
            Prelude.<$> (x Prelude..: "AttributeName")
            Prelude.<*> (x Prelude..: "AttributeType")
      )

instance Prelude.Hashable AttributeDefinition

instance Prelude.NFData AttributeDefinition

instance Prelude.ToJSON AttributeDefinition where
  toJSON AttributeDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Prelude..= attributeName),
            Prelude.Just
              ("AttributeType" Prelude..= attributeType)
          ]
      )
