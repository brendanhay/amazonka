{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectAttributes
  ( BatchGetObjectAttributes (..),

    -- * Smart constructor
    mkBatchGetObjectAttributes,

    -- * Lenses
    bgoaObjectReference,
    bgoaSchemaFacet,
    bgoaAttributeNames,
  )
where

import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Retrieves attributes within a facet that are associated with an object inside an 'BatchRead' operation. For more information, see 'GetObjectAttributes' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchGetObjectAttributes' smart constructor.
data BatchGetObjectAttributes = BatchGetObjectAttributes'
  { objectReference ::
      ObjectReference,
    schemaFacet :: SchemaFacet,
    attributeNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetObjectAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - List of attribute names whose values will be retrieved.
-- * 'objectReference' - Reference that identifies the object whose attributes will be retrieved.
-- * 'schemaFacet' - Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
mkBatchGetObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  -- | 'schemaFacet'
  SchemaFacet ->
  BatchGetObjectAttributes
mkBatchGetObjectAttributes pObjectReference_ pSchemaFacet_ =
  BatchGetObjectAttributes'
    { objectReference = pObjectReference_,
      schemaFacet = pSchemaFacet_,
      attributeNames = Lude.mempty
    }

-- | Reference that identifies the object whose attributes will be retrieved.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoaObjectReference :: Lens.Lens' BatchGetObjectAttributes ObjectReference
bgoaObjectReference = Lens.lens (objectReference :: BatchGetObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: BatchGetObjectAttributes)
{-# DEPRECATED bgoaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoaSchemaFacet :: Lens.Lens' BatchGetObjectAttributes SchemaFacet
bgoaSchemaFacet = Lens.lens (schemaFacet :: BatchGetObjectAttributes -> SchemaFacet) (\s a -> s {schemaFacet = a} :: BatchGetObjectAttributes)
{-# DEPRECATED bgoaSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | List of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoaAttributeNames :: Lens.Lens' BatchGetObjectAttributes [Lude.Text]
bgoaAttributeNames = Lens.lens (attributeNames :: BatchGetObjectAttributes -> [Lude.Text]) (\s a -> s {attributeNames = a} :: BatchGetObjectAttributes)
{-# DEPRECATED bgoaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.ToJSON BatchGetObjectAttributes where
  toJSON BatchGetObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ObjectReference" Lude..= objectReference),
            Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("AttributeNames" Lude..= attributeNames)
          ]
      )
