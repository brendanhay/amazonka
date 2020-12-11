{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpdateTypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.UpdateTypedLinkFacet
  ( -- * Creating a request
    UpdateTypedLinkFacet (..),
    mkUpdateTypedLinkFacet,

    -- ** Request lenses
    utlfSchemaARN,
    utlfName,
    utlfAttributeUpdates,
    utlfIdentityAttributeOrder,

    -- * Destructuring the response
    UpdateTypedLinkFacetResponse (..),
    mkUpdateTypedLinkFacetResponse,

    -- ** Response lenses
    utlfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTypedLinkFacet' smart constructor.
data UpdateTypedLinkFacet = UpdateTypedLinkFacet'
  { schemaARN ::
      Lude.Text,
    name :: Lude.Text,
    attributeUpdates ::
      [TypedLinkFacetAttributeUpdate],
    identityAttributeOrder :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTypedLinkFacet' with the minimum fields required to make a request.
--
-- * 'attributeUpdates' - Attributes update structure.
-- * 'identityAttributeOrder' - The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'name' - The unique name of the typed link facet.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
mkUpdateTypedLinkFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  UpdateTypedLinkFacet
mkUpdateTypedLinkFacet pSchemaARN_ pName_ =
  UpdateTypedLinkFacet'
    { schemaARN = pSchemaARN_,
      name = pName_,
      attributeUpdates = Lude.mempty,
      identityAttributeOrder = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfSchemaARN :: Lens.Lens' UpdateTypedLinkFacet Lude.Text
utlfSchemaARN = Lens.lens (schemaARN :: UpdateTypedLinkFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: UpdateTypedLinkFacet)
{-# DEPRECATED utlfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfName :: Lens.Lens' UpdateTypedLinkFacet Lude.Text
utlfName = Lens.lens (name :: UpdateTypedLinkFacet -> Lude.Text) (\s a -> s {name = a} :: UpdateTypedLinkFacet)
{-# DEPRECATED utlfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Attributes update structure.
--
-- /Note:/ Consider using 'attributeUpdates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfAttributeUpdates :: Lens.Lens' UpdateTypedLinkFacet [TypedLinkFacetAttributeUpdate]
utlfAttributeUpdates = Lens.lens (attributeUpdates :: UpdateTypedLinkFacet -> [TypedLinkFacetAttributeUpdate]) (\s a -> s {attributeUpdates = a} :: UpdateTypedLinkFacet)
{-# DEPRECATED utlfAttributeUpdates "Use generic-lens or generic-optics with 'attributeUpdates' instead." #-}

-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to a typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfIdentityAttributeOrder :: Lens.Lens' UpdateTypedLinkFacet [Lude.Text]
utlfIdentityAttributeOrder = Lens.lens (identityAttributeOrder :: UpdateTypedLinkFacet -> [Lude.Text]) (\s a -> s {identityAttributeOrder = a} :: UpdateTypedLinkFacet)
{-# DEPRECATED utlfIdentityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead." #-}

instance Lude.AWSRequest UpdateTypedLinkFacet where
  type Rs UpdateTypedLinkFacet = UpdateTypedLinkFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateTypedLinkFacetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTypedLinkFacet where
  toHeaders UpdateTypedLinkFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON UpdateTypedLinkFacet where
  toJSON UpdateTypedLinkFacet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("AttributeUpdates" Lude..= attributeUpdates),
            Lude.Just
              ("IdentityAttributeOrder" Lude..= identityAttributeOrder)
          ]
      )

instance Lude.ToPath UpdateTypedLinkFacet where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/facet"

instance Lude.ToQuery UpdateTypedLinkFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTypedLinkFacetResponse' smart constructor.
newtype UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse'
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

-- | Creates a value of 'UpdateTypedLinkFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateTypedLinkFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTypedLinkFacetResponse
mkUpdateTypedLinkFacetResponse pResponseStatus_ =
  UpdateTypedLinkFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utlfrsResponseStatus :: Lens.Lens' UpdateTypedLinkFacetResponse Lude.Int
utlfrsResponseStatus = Lens.lens (responseStatus :: UpdateTypedLinkFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTypedLinkFacetResponse)
{-# DEPRECATED utlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
