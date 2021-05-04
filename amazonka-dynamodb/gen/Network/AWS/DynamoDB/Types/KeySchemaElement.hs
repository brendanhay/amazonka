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
-- Module      : Network.AWS.DynamoDB.Types.KeySchemaElement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KeySchemaElement where

import Network.AWS.DynamoDB.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents /a single element/ of a key schema. A key schema specifies
-- the attributes that make up the primary key of a table, or the key
-- attributes of an index.
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary
-- key. For example, a simple primary key would be represented by one
-- @KeySchemaElement@ (for the partition key). A composite primary key
-- would require one @KeySchemaElement@ for the partition key, and another
-- @KeySchemaElement@ for the sort key.
--
-- A @KeySchemaElement@ must be a scalar, top-level attribute (not a nested
-- attribute). The data type must be one of String, Number, or Binary. The
-- attribute cannot be nested within a List or a Map.
--
-- /See:/ 'newKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a key attribute.
    attributeName :: Prelude.Text,
    -- | The role that this key attribute will assume:
    --
    -- -   @HASH@ - partition key
    --
    -- -   @RANGE@ - sort key
    --
    -- The partition key of an item is also known as its /hash attribute/. The
    -- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
    -- hash function to evenly distribute data items across partitions, based
    -- on their partition key values.
    --
    -- The sort key of an item is also known as its /range attribute/. The term
    -- \"range attribute\" derives from the way DynamoDB stores items with the
    -- same partition key physically close together, in sorted order by the
    -- sort key value.
    keyType :: KeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KeySchemaElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'keySchemaElement_attributeName' - The name of a key attribute.
--
-- 'keyType', 'keySchemaElement_keyType' - The role that this key attribute will assume:
--
-- -   @HASH@ - partition key
--
-- -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
newKeySchemaElement ::
  -- | 'attributeName'
  Prelude.Text ->
  -- | 'keyType'
  KeyType ->
  KeySchemaElement
newKeySchemaElement pAttributeName_ pKeyType_ =
  KeySchemaElement'
    { attributeName = pAttributeName_,
      keyType = pKeyType_
    }

-- | The name of a key attribute.
keySchemaElement_attributeName :: Lens.Lens' KeySchemaElement Prelude.Text
keySchemaElement_attributeName = Lens.lens (\KeySchemaElement' {attributeName} -> attributeName) (\s@KeySchemaElement' {} a -> s {attributeName = a} :: KeySchemaElement)

-- | The role that this key attribute will assume:
--
-- -   @HASH@ - partition key
--
-- -   @RANGE@ - sort key
--
-- The partition key of an item is also known as its /hash attribute/. The
-- term \"hash attribute\" derives from DynamoDB\'s usage of an internal
-- hash function to evenly distribute data items across partitions, based
-- on their partition key values.
--
-- The sort key of an item is also known as its /range attribute/. The term
-- \"range attribute\" derives from the way DynamoDB stores items with the
-- same partition key physically close together, in sorted order by the
-- sort key value.
keySchemaElement_keyType :: Lens.Lens' KeySchemaElement KeyType
keySchemaElement_keyType = Lens.lens (\KeySchemaElement' {keyType} -> keyType) (\s@KeySchemaElement' {} a -> s {keyType = a} :: KeySchemaElement)

instance Prelude.FromJSON KeySchemaElement where
  parseJSON =
    Prelude.withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement'
            Prelude.<$> (x Prelude..: "AttributeName")
            Prelude.<*> (x Prelude..: "KeyType")
      )

instance Prelude.Hashable KeySchemaElement

instance Prelude.NFData KeySchemaElement

instance Prelude.ToJSON KeySchemaElement where
  toJSON KeySchemaElement' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Prelude..= attributeName),
            Prelude.Just ("KeyType" Prelude..= keyType)
          ]
      )
