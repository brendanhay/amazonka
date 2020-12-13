{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new 'Facet' in a schema. Facet creation is allowed only in development or applied schemas.
module Network.AWS.CloudDirectory.CreateFacet
  ( -- * Creating a request
    CreateFacet (..),
    mkCreateFacet,

    -- ** Request lenses
    cfFacetStyle,
    cfObjectType,
    cfSchemaARN,
    cfAttributes,
    cfName,

    -- * Destructuring the response
    CreateFacetResponse (..),
    mkCreateFacetResponse,

    -- ** Response lenses
    cfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFacet' smart constructor.
data CreateFacet = CreateFacet'
  { -- | There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
    facetStyle :: Lude.Maybe FacetStyle,
    -- | Specifies whether a given object created from this facet is of type node, leaf node, policy or index.
    --
    --
    --     * Node: Can have multiple children but one parent.
    --
    --
    --
    --     * Leaf node: Cannot have children but can have multiple parents.
    --
    --
    --
    --     * Policy: Allows you to store a policy document and policy type. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
    --
    --
    --
    --     * Index: Can be created with the Index API.
    objectType :: Lude.Maybe ObjectType,
    -- | The schema ARN in which the new 'Facet' will be created. For more information, see 'arns' .
    schemaARN :: Lude.Text,
    -- | The attributes that are associated with the 'Facet' .
    attributes :: Lude.Maybe [FacetAttribute],
    -- | The name of the 'Facet' , which is unique for a given schema.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFacet' with the minimum fields required to make a request.
--
-- * 'facetStyle' - There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
-- * 'objectType' - Specifies whether a given object created from this facet is of type node, leaf node, policy or index.
--
--
--     * Node: Can have multiple children but one parent.
--
--
--
--     * Leaf node: Cannot have children but can have multiple parents.
--
--
--
--     * Policy: Allows you to store a policy document and policy type. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
--
--
--     * Index: Can be created with the Index API.
--
--
-- * 'schemaARN' - The schema ARN in which the new 'Facet' will be created. For more information, see 'arns' .
-- * 'attributes' - The attributes that are associated with the 'Facet' .
-- * 'name' - The name of the 'Facet' , which is unique for a given schema.
mkCreateFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  CreateFacet
mkCreateFacet pSchemaARN_ pName_ =
  CreateFacet'
    { facetStyle = Lude.Nothing,
      objectType = Lude.Nothing,
      schemaARN = pSchemaARN_,
      attributes = Lude.Nothing,
      name = pName_
    }

-- | There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
--
-- /Note:/ Consider using 'facetStyle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfFacetStyle :: Lens.Lens' CreateFacet (Lude.Maybe FacetStyle)
cfFacetStyle = Lens.lens (facetStyle :: CreateFacet -> Lude.Maybe FacetStyle) (\s a -> s {facetStyle = a} :: CreateFacet)
{-# DEPRECATED cfFacetStyle "Use generic-lens or generic-optics with 'facetStyle' instead." #-}

-- | Specifies whether a given object created from this facet is of type node, leaf node, policy or index.
--
--
--     * Node: Can have multiple children but one parent.
--
--
--
--     * Leaf node: Cannot have children but can have multiple parents.
--
--
--
--     * Policy: Allows you to store a policy document and policy type. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies> .
--
--
--
--     * Index: Can be created with the Index API.
--
--
--
-- /Note:/ Consider using 'objectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfObjectType :: Lens.Lens' CreateFacet (Lude.Maybe ObjectType)
cfObjectType = Lens.lens (objectType :: CreateFacet -> Lude.Maybe ObjectType) (\s a -> s {objectType = a} :: CreateFacet)
{-# DEPRECATED cfObjectType "Use generic-lens or generic-optics with 'objectType' instead." #-}

-- | The schema ARN in which the new 'Facet' will be created. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfSchemaARN :: Lens.Lens' CreateFacet Lude.Text
cfSchemaARN = Lens.lens (schemaARN :: CreateFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: CreateFacet)
{-# DEPRECATED cfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The attributes that are associated with the 'Facet' .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAttributes :: Lens.Lens' CreateFacet (Lude.Maybe [FacetAttribute])
cfAttributes = Lens.lens (attributes :: CreateFacet -> Lude.Maybe [FacetAttribute]) (\s a -> s {attributes = a} :: CreateFacet)
{-# DEPRECATED cfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The name of the 'Facet' , which is unique for a given schema.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFacet Lude.Text
cfName = Lens.lens (name :: CreateFacet -> Lude.Text) (\s a -> s {name = a} :: CreateFacet)
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateFacet where
  type Rs CreateFacet = CreateFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateFacetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateFacet where
  toHeaders CreateFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON CreateFacet where
  toJSON CreateFacet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FacetStyle" Lude..=) Lude.<$> facetStyle,
            ("ObjectType" Lude..=) Lude.<$> objectType,
            ("Attributes" Lude..=) Lude.<$> attributes,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateFacet where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/facet/create"

instance Lude.ToQuery CreateFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateFacetResponse' smart constructor.
newtype CreateFacetResponse = CreateFacetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateFacetResponse
mkCreateFacetResponse pResponseStatus_ =
  CreateFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrsResponseStatus :: Lens.Lens' CreateFacetResponse Lude.Int
cfrsResponseStatus = Lens.lens (responseStatus :: CreateFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateFacetResponse)
{-# DEPRECATED cfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
