{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an object in a 'Directory' . Additionally attaches the object to a parent, if a parent reference and @LinkName@ is specified. An object is simply a collection of 'Facet' attributes. You can also use this API call to create a policy object, if the facet from which you create the object is a policy facet.
module Network.AWS.CloudDirectory.CreateObject
  ( -- * Creating a request
    CreateObject (..),
    mkCreateObject,

    -- ** Request lenses
    coParentReference,
    coObjectAttributeList,
    coLinkName,
    coDirectoryARN,
    coSchemaFacets,

    -- * Destructuring the response
    CreateObjectResponse (..),
    mkCreateObjectResponse,

    -- ** Response lenses
    corsObjectIdentifier,
    corsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateObject' smart constructor.
data CreateObject = CreateObject'
  { parentReference ::
      Lude.Maybe ObjectReference,
    objectAttributeList :: Lude.Maybe [AttributeKeyAndValue],
    linkName :: Lude.Maybe Lude.Text,
    directoryARN :: Lude.Text,
    schemaFacets :: [SchemaFacet]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' in which the object will be created. For more information, see 'arns' .
-- * 'linkName' - The name of link that is used to attach this object to a parent.
-- * 'objectAttributeList' - The attribute map whose attribute ARN contains the key and attribute value as the map value.
-- * 'parentReference' - If specified, the parent reference to which this object will be attached.
-- * 'schemaFacets' - A list of schema facets to be associated with the object. Do not provide minor version components. See 'SchemaFacet' for details.
mkCreateObject ::
  -- | 'directoryARN'
  Lude.Text ->
  CreateObject
mkCreateObject pDirectoryARN_ =
  CreateObject'
    { parentReference = Lude.Nothing,
      objectAttributeList = Lude.Nothing,
      linkName = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      schemaFacets = Lude.mempty
    }

-- | If specified, the parent reference to which this object will be attached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coParentReference :: Lens.Lens' CreateObject (Lude.Maybe ObjectReference)
coParentReference = Lens.lens (parentReference :: CreateObject -> Lude.Maybe ObjectReference) (\s a -> s {parentReference = a} :: CreateObject)
{-# DEPRECATED coParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The attribute map whose attribute ARN contains the key and attribute value as the map value.
--
-- /Note:/ Consider using 'objectAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coObjectAttributeList :: Lens.Lens' CreateObject (Lude.Maybe [AttributeKeyAndValue])
coObjectAttributeList = Lens.lens (objectAttributeList :: CreateObject -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {objectAttributeList = a} :: CreateObject)
{-# DEPRECATED coObjectAttributeList "Use generic-lens or generic-optics with 'objectAttributeList' instead." #-}

-- | The name of link that is used to attach this object to a parent.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coLinkName :: Lens.Lens' CreateObject (Lude.Maybe Lude.Text)
coLinkName = Lens.lens (linkName :: CreateObject -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: CreateObject)
{-# DEPRECATED coLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' in which the object will be created. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coDirectoryARN :: Lens.Lens' CreateObject Lude.Text
coDirectoryARN = Lens.lens (directoryARN :: CreateObject -> Lude.Text) (\s a -> s {directoryARN = a} :: CreateObject)
{-# DEPRECATED coDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A list of schema facets to be associated with the object. Do not provide minor version components. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coSchemaFacets :: Lens.Lens' CreateObject [SchemaFacet]
coSchemaFacets = Lens.lens (schemaFacets :: CreateObject -> [SchemaFacet]) (\s a -> s {schemaFacets = a} :: CreateObject)
{-# DEPRECATED coSchemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead." #-}

instance Lude.AWSRequest CreateObject where
  type Rs CreateObject = CreateObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateObjectResponse'
            Lude.<$> (x Lude..?> "ObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateObject where
  toHeaders CreateObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON CreateObject where
  toJSON CreateObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentReference" Lude..=) Lude.<$> parentReference,
            ("ObjectAttributeList" Lude..=) Lude.<$> objectAttributeList,
            ("LinkName" Lude..=) Lude.<$> linkName,
            Lude.Just ("SchemaFacets" Lude..= schemaFacets)
          ]
      )

instance Lude.ToPath CreateObject where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/object"

instance Lude.ToQuery CreateObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateObjectResponse' smart constructor.
data CreateObjectResponse = CreateObjectResponse'
  { objectIdentifier ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateObjectResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The identifier that is associated with the object.
-- * 'responseStatus' - The response status code.
mkCreateObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateObjectResponse
mkCreateObjectResponse pResponseStatus_ =
  CreateObjectResponse'
    { objectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier that is associated with the object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsObjectIdentifier :: Lens.Lens' CreateObjectResponse (Lude.Maybe Lude.Text)
corsObjectIdentifier = Lens.lens (objectIdentifier :: CreateObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: CreateObjectResponse)
{-# DEPRECATED corsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
corsResponseStatus :: Lens.Lens' CreateObjectResponse Lude.Int
corsResponseStatus = Lens.lens (responseStatus :: CreateObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateObjectResponse)
{-# DEPRECATED corsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
