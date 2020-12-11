{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AddFacetToObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Facet' to an object. An object can have more than one facet applied on it.
module Network.AWS.CloudDirectory.AddFacetToObject
  ( -- * Creating a request
    AddFacetToObject (..),
    mkAddFacetToObject,

    -- ** Request lenses
    aftoObjectAttributeList,
    aftoDirectoryARN,
    aftoSchemaFacet,
    aftoObjectReference,

    -- * Destructuring the response
    AddFacetToObjectResponse (..),
    mkAddFacetToObjectResponse,

    -- ** Response lenses
    aftorsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddFacetToObject' smart constructor.
data AddFacetToObject = AddFacetToObject'
  { objectAttributeList ::
      Lude.Maybe [AttributeKeyAndValue],
    directoryARN :: Lude.Text,
    schemaFacet :: SchemaFacet,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddFacetToObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
-- * 'objectAttributeList' - Attributes on the facet that you are adding to the object.
-- * 'objectReference' - A reference to the object you are adding the specified facet to.
-- * 'schemaFacet' - Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
mkAddFacetToObject ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  AddFacetToObject
mkAddFacetToObject pDirectoryARN_ pSchemaFacet_ pObjectReference_ =
  AddFacetToObject'
    { objectAttributeList = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      schemaFacet = pSchemaFacet_,
      objectReference = pObjectReference_
    }

-- | Attributes on the facet that you are adding to the object.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoObjectAttributeList :: Lens.Lens' AddFacetToObject (Lude.Maybe [AttributeKeyAndValue])
aftoObjectAttributeList = Lens.lens (objectAttributeList :: AddFacetToObject -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {objectAttributeList = a} :: AddFacetToObject)
{-# DEPRECATED aftoObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoDirectoryARN :: Lens.Lens' AddFacetToObject Lude.Text
aftoDirectoryARN = Lens.lens (directoryARN :: AddFacetToObject -> Lude.Text) (\s a -> s {directoryARN = a} :: AddFacetToObject)
{-# DEPRECATED aftoDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Identifiers for the facet that you are adding to the object. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoSchemaFacet :: Lens.Lens' AddFacetToObject SchemaFacet
aftoSchemaFacet = Lens.lens (schemaFacet :: AddFacetToObject -> SchemaFacet) (\s a -> s {schemaFacet = a} :: AddFacetToObject)
{-# DEPRECATED aftoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | A reference to the object you are adding the specified facet to.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftoObjectReference :: Lens.Lens' AddFacetToObject ObjectReference
aftoObjectReference = Lens.lens (objectReference :: AddFacetToObject -> ObjectReference) (\s a -> s {objectReference = a} :: AddFacetToObject)
{-# DEPRECATED aftoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest AddFacetToObject where
  type Rs AddFacetToObject = AddFacetToObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddFacetToObjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddFacetToObject where
  toHeaders AddFacetToObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON AddFacetToObject where
  toJSON AddFacetToObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ObjectAttributeList" Lude..=) Lude.<$> objectAttributeList,
            Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath AddFacetToObject where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/facets"

instance Lude.ToQuery AddFacetToObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddFacetToObjectResponse' smart constructor.
newtype AddFacetToObjectResponse = AddFacetToObjectResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddFacetToObjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddFacetToObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddFacetToObjectResponse
mkAddFacetToObjectResponse pResponseStatus_ =
  AddFacetToObjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aftorsResponseStatus :: Lens.Lens' AddFacetToObjectResponse Lude.Int
aftorsResponseStatus = Lens.lens (responseStatus :: AddFacetToObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddFacetToObjectResponse)
{-# DEPRECATED aftorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
