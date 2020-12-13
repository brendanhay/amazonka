{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KeySchemaElement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KeySchemaElement
  ( KeySchemaElement (..),

    -- * Smart constructor
    mkKeySchemaElement,

    -- * Lenses
    kseKeyType,
    kseAttributeName,
  )
where

import Network.AWS.DynamoDB.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents /a single element/ of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary key. For example, a simple primary key would be represented by one @KeySchemaElement@ (for the partition key). A composite primary key would require one @KeySchemaElement@ for the partition key, and another @KeySchemaElement@ for the sort key.
-- A @KeySchemaElement@ must be a scalar, top-level attribute (not a nested attribute). The data type must be one of String, Number, or Binary. The attribute cannot be nested within a List or a Map.
--
-- /See:/ 'mkKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The role that this key attribute will assume:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keyType :: KeyType,
    -- | The name of a key attribute.
    attributeName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeySchemaElement' with the minimum fields required to make a request.
--
-- * 'keyType' - The role that this key attribute will assume:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
-- * 'attributeName' - The name of a key attribute.
mkKeySchemaElement ::
  -- | 'keyType'
  KeyType ->
  -- | 'attributeName'
  Lude.Text ->
  KeySchemaElement
mkKeySchemaElement pKeyType_ pAttributeName_ =
  KeySchemaElement'
    { keyType = pKeyType_,
      attributeName = pAttributeName_
    }

-- | The role that this key attribute will assume:
--
--
--     * @HASH@ - partition key
--
--
--     * @RANGE@ - sort key
--
--
--
-- /Note:/ Consider using 'keyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseKeyType :: Lens.Lens' KeySchemaElement KeyType
kseKeyType = Lens.lens (keyType :: KeySchemaElement -> KeyType) (\s a -> s {keyType = a} :: KeySchemaElement)
{-# DEPRECATED kseKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

-- | The name of a key attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseAttributeName :: Lens.Lens' KeySchemaElement Lude.Text
kseAttributeName = Lens.lens (attributeName :: KeySchemaElement -> Lude.Text) (\s a -> s {attributeName = a} :: KeySchemaElement)
{-# DEPRECATED kseAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.FromJSON KeySchemaElement where
  parseJSON =
    Lude.withObject
      "KeySchemaElement"
      ( \x ->
          KeySchemaElement'
            Lude.<$> (x Lude..: "KeyType") Lude.<*> (x Lude..: "AttributeName")
      )

instance Lude.ToJSON KeySchemaElement where
  toJSON KeySchemaElement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyType" Lude..= keyType),
            Lude.Just ("AttributeName" Lude..= attributeName)
          ]
      )
