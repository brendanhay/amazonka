{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetObjectAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes within a facet that are associated with an object.
module Network.AWS.CloudDirectory.GetObjectAttributes
  ( -- * Creating a request
    GetObjectAttributes (..),
    mkGetObjectAttributes,

    -- ** Request lenses
    goaConsistencyLevel,
    goaDirectoryARN,
    goaObjectReference,
    goaSchemaFacet,
    goaAttributeNames,

    -- * Destructuring the response
    GetObjectAttributesResponse (..),
    mkGetObjectAttributesResponse,

    -- ** Response lenses
    goarsAttributes,
    goarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetObjectAttributes' smart constructor.
data GetObjectAttributes = GetObjectAttributes'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    directoryARN :: Lude.Text,
    objectReference :: ObjectReference,
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

-- | Creates a value of 'GetObjectAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - List of attribute names whose values will be retrieved.
-- * 'consistencyLevel' - The consistency level at which to retrieve the attributes on an object.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
-- * 'objectReference' - Reference that identifies the object whose attributes will be retrieved.
-- * 'schemaFacet' - Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
mkGetObjectAttributes ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  -- | 'schemaFacet'
  SchemaFacet ->
  GetObjectAttributes
mkGetObjectAttributes
  pDirectoryARN_
  pObjectReference_
  pSchemaFacet_ =
    GetObjectAttributes'
      { consistencyLevel = Lude.Nothing,
        directoryARN = pDirectoryARN_,
        objectReference = pObjectReference_,
        schemaFacet = pSchemaFacet_,
        attributeNames = Lude.mempty
      }

-- | The consistency level at which to retrieve the attributes on an object.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaConsistencyLevel :: Lens.Lens' GetObjectAttributes (Lude.Maybe ConsistencyLevel)
goaConsistencyLevel = Lens.lens (consistencyLevel :: GetObjectAttributes -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: GetObjectAttributes)
{-# DEPRECATED goaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where the object resides.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaDirectoryARN :: Lens.Lens' GetObjectAttributes Lude.Text
goaDirectoryARN = Lens.lens (directoryARN :: GetObjectAttributes -> Lude.Text) (\s a -> s {directoryARN = a} :: GetObjectAttributes)
{-# DEPRECATED goaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Reference that identifies the object whose attributes will be retrieved.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaObjectReference :: Lens.Lens' GetObjectAttributes ObjectReference
goaObjectReference = Lens.lens (objectReference :: GetObjectAttributes -> ObjectReference) (\s a -> s {objectReference = a} :: GetObjectAttributes)
{-# DEPRECATED goaObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaSchemaFacet :: Lens.Lens' GetObjectAttributes SchemaFacet
goaSchemaFacet = Lens.lens (schemaFacet :: GetObjectAttributes -> SchemaFacet) (\s a -> s {schemaFacet = a} :: GetObjectAttributes)
{-# DEPRECATED goaSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | List of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaAttributeNames :: Lens.Lens' GetObjectAttributes [Lude.Text]
goaAttributeNames = Lens.lens (attributeNames :: GetObjectAttributes -> [Lude.Text]) (\s a -> s {attributeNames = a} :: GetObjectAttributes)
{-# DEPRECATED goaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.AWSRequest GetObjectAttributes where
  type Rs GetObjectAttributes = GetObjectAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetObjectAttributesResponse'
            Lude.<$> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectAttributes where
  toHeaders GetObjectAttributes' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON GetObjectAttributes where
  toJSON GetObjectAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ObjectReference" Lude..= objectReference),
            Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("AttributeNames" Lude..= attributeNames)
          ]
      )

instance Lude.ToPath GetObjectAttributes where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/object/attributes/get"

instance Lude.ToQuery GetObjectAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetObjectAttributesResponse' smart constructor.
data GetObjectAttributesResponse = GetObjectAttributesResponse'
  { attributes ::
      Lude.Maybe [AttributeKeyAndValue],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes that are associated with the object.
-- * 'responseStatus' - The response status code.
mkGetObjectAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectAttributesResponse
mkGetObjectAttributesResponse pResponseStatus_ =
  GetObjectAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attributes that are associated with the object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsAttributes :: Lens.Lens' GetObjectAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
goarsAttributes = Lens.lens (attributes :: GetObjectAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: GetObjectAttributesResponse)
{-# DEPRECATED goarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarsResponseStatus :: Lens.Lens' GetObjectAttributesResponse Lude.Int
goarsResponseStatus = Lens.lens (responseStatus :: GetObjectAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectAttributesResponse)
{-# DEPRECATED goarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
