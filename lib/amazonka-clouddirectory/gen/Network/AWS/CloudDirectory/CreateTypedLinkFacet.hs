{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateTypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.CreateTypedLinkFacet
  ( -- * Creating a request
    CreateTypedLinkFacet (..),
    mkCreateTypedLinkFacet,

    -- ** Request lenses
    ctlfSchemaARN,
    ctlfFacet,

    -- * Destructuring the response
    CreateTypedLinkFacetResponse (..),
    mkCreateTypedLinkFacetResponse,

    -- ** Response lenses
    ctlfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTypedLinkFacet' smart constructor.
data CreateTypedLinkFacet = CreateTypedLinkFacet'
  { schemaARN ::
      Lude.Text,
    facet :: TypedLinkFacet
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTypedLinkFacet' with the minimum fields required to make a request.
--
-- * 'facet' - 'Facet' structure that is associated with the typed link facet.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
mkCreateTypedLinkFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'facet'
  TypedLinkFacet ->
  CreateTypedLinkFacet
mkCreateTypedLinkFacet pSchemaARN_ pFacet_ =
  CreateTypedLinkFacet' {schemaARN = pSchemaARN_, facet = pFacet_}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfSchemaARN :: Lens.Lens' CreateTypedLinkFacet Lude.Text
ctlfSchemaARN = Lens.lens (schemaARN :: CreateTypedLinkFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: CreateTypedLinkFacet)
{-# DEPRECATED ctlfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | 'Facet' structure that is associated with the typed link facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfFacet :: Lens.Lens' CreateTypedLinkFacet TypedLinkFacet
ctlfFacet = Lens.lens (facet :: CreateTypedLinkFacet -> TypedLinkFacet) (\s a -> s {facet = a} :: CreateTypedLinkFacet)
{-# DEPRECATED ctlfFacet "Use generic-lens or generic-optics with 'facet' instead." #-}

instance Lude.AWSRequest CreateTypedLinkFacet where
  type Rs CreateTypedLinkFacet = CreateTypedLinkFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateTypedLinkFacetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTypedLinkFacet where
  toHeaders CreateTypedLinkFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON CreateTypedLinkFacet where
  toJSON CreateTypedLinkFacet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Facet" Lude..= facet)])

instance Lude.ToPath CreateTypedLinkFacet where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/create"

instance Lude.ToQuery CreateTypedLinkFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTypedLinkFacetResponse' smart constructor.
newtype CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse'
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

-- | Creates a value of 'CreateTypedLinkFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateTypedLinkFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTypedLinkFacetResponse
mkCreateTypedLinkFacetResponse pResponseStatus_ =
  CreateTypedLinkFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctlfrsResponseStatus :: Lens.Lens' CreateTypedLinkFacetResponse Lude.Int
ctlfrsResponseStatus = Lens.lens (responseStatus :: CreateTypedLinkFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTypedLinkFacetResponse)
{-# DEPRECATED ctlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
