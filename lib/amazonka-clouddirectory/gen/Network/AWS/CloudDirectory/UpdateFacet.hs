{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Does the following:
--
--
--     * Adds new @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
--     * Updates existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
--
--
--     * Deletes existing @Attributes@ , @Rules@ , or @ObjectTypes@ .
module Network.AWS.CloudDirectory.UpdateFacet
  ( -- * Creating a request
    UpdateFacet (..),
    mkUpdateFacet,

    -- ** Request lenses
    ufObjectType,
    ufAttributeUpdates,
    ufSchemaARN,
    ufName,

    -- * Destructuring the response
    UpdateFacetResponse (..),
    mkUpdateFacetResponse,

    -- ** Response lenses
    ufrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { objectType ::
      Lude.Maybe ObjectType,
    attributeUpdates :: Lude.Maybe [FacetAttributeUpdate],
    schemaARN :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateFacet' with the minimum fields required to make a request.
--
-- * 'attributeUpdates' - List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform.
-- * 'name' - The name of the facet.
-- * 'objectType' - The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
mkUpdateFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateFacet
mkUpdateFacet pSchemaARN_ pName_ =
  UpdateFacet'
    { objectType = Lude.Nothing,
      attributeUpdates = Lude.Nothing,
      schemaARN = pSchemaARN_,
      name = pName_
    }

-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- /Note:/ Consider using 'objectType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufObjectType :: Lens.Lens' UpdateFacet (Lude.Maybe ObjectType)
ufObjectType = Lens.lens (objectType :: UpdateFacet -> Lude.Maybe ObjectType) (\s a -> s {objectType = a} :: UpdateFacet)
{-# DEPRECATED ufObjectType "Use generic-lens or generic-optics with 'objectType' instead." #-}

-- | List of attributes that need to be updated in a given schema 'Facet' . Each attribute is followed by @AttributeAction@ , which specifies the type of update operation to perform.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufAttributeUpdates :: Lens.Lens' UpdateFacet (Lude.Maybe [FacetAttributeUpdate])
ufAttributeUpdates = Lens.lens (attributeUpdates :: UpdateFacet -> Lude.Maybe [FacetAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: UpdateFacet)
{-# DEPRECATED ufAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufSchemaARN :: Lens.Lens' UpdateFacet Lude.Text
ufSchemaARN = Lens.lens (schemaARN :: UpdateFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: UpdateFacet)
{-# DEPRECATED ufSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufName :: Lens.Lens' UpdateFacet Lude.Text
ufName = Lens.lens (name :: UpdateFacet -> Lude.Text) (\s a -> s {name = a} :: UpdateFacet)
{-# DEPRECATED ufName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateFacet where
  type Rs UpdateFacet = UpdateFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateFacetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateFacet where
  toHeaders UpdateFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON UpdateFacet where
  toJSON UpdateFacet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ObjectType" Lude..=) Lude.<$> objectType,
            ("AttributeUpdates" Lude..=) Lude.<$> attributeUpdates,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateFacet where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/facet"

instance Lude.ToQuery UpdateFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateFacetResponse' smart constructor.
newtype UpdateFacetResponse = UpdateFacetResponse'
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

-- | Creates a value of 'UpdateFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateFacetResponse
mkUpdateFacetResponse pResponseStatus_ =
  UpdateFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ufrsResponseStatus :: Lens.Lens' UpdateFacetResponse Lude.Int
ufrsResponseStatus = Lens.lens (responseStatus :: UpdateFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateFacetResponse)
{-# DEPRECATED ufrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
