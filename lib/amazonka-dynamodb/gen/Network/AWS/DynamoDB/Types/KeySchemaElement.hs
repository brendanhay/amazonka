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
    kseAttributeName,
    kseKeyType,
  )
where

import qualified Network.AWS.DynamoDB.Types.KeySchemaAttributeName as Types
import qualified Network.AWS.DynamoDB.Types.KeyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents /a single element/ of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.
--
-- A @KeySchemaElement@ represents exactly one attribute of the primary key. For example, a simple primary key would be represented by one @KeySchemaElement@ (for the partition key). A composite primary key would require one @KeySchemaElement@ for the partition key, and another @KeySchemaElement@ for the sort key.
-- A @KeySchemaElement@ must be a scalar, top-level attribute (not a nested attribute). The data type must be one of String, Number, or Binary. The attribute cannot be nested within a List or a Map.
--
-- /See:/ 'mkKeySchemaElement' smart constructor.
data KeySchemaElement = KeySchemaElement'
  { -- | The name of a key attribute.
    attributeName :: Types.KeySchemaAttributeName,
    -- | The role that this key attribute will assume:
    --
    --
    --     * @HASH@ - partition key
    --
    --
    --     * @RANGE@ - sort key
    keyType :: Types.KeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeySchemaElement' value with any optional fields omitted.
mkKeySchemaElement ::
  -- | 'attributeName'
  Types.KeySchemaAttributeName ->
  -- | 'keyType'
  Types.KeyType ->
  KeySchemaElement
mkKeySchemaElement attributeName keyType =
  KeySchemaElement' {attributeName, keyType}

-- | The name of a key attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kseAttributeName :: Lens.Lens' KeySchemaElement Types.KeySchemaAttributeName
kseAttributeName = Lens.field @"attributeName"
{-# DEPRECATED kseAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

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
kseKeyType :: Lens.Lens' KeySchemaElement Types.KeyType
kseKeyType = Lens.field @"keyType"
{-# DEPRECATED kseKeyType "Use generic-lens or generic-optics with 'keyType' instead." #-}

instance Core.FromJSON KeySchemaElement where
  toJSON KeySchemaElement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AttributeName" Core..= attributeName),
            Core.Just ("KeyType" Core..= keyType)
          ]
      )

instance Core.FromJSON KeySchemaElement where
  parseJSON =
    Core.withObject "KeySchemaElement" Core.$
      \x ->
        KeySchemaElement'
          Core.<$> (x Core..: "AttributeName") Core.<*> (x Core..: "KeyType")
