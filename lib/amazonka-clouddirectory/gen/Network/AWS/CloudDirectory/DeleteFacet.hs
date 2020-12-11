{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given 'Facet' . All attributes and 'Rule' s that are associated with the facet will be deleted. Only development schema facets are allowed deletion.
module Network.AWS.CloudDirectory.DeleteFacet
  ( -- * Creating a request
    DeleteFacet (..),
    mkDeleteFacet,

    -- ** Request lenses
    dfSchemaARN,
    dfName,

    -- * Destructuring the response
    DeleteFacetResponse (..),
    mkDeleteFacetResponse,

    -- ** Response lenses
    dfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFacet' smart constructor.
data DeleteFacet = DeleteFacet'
  { schemaARN :: Lude.Text,
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

-- | Creates a value of 'DeleteFacet' with the minimum fields required to make a request.
--
-- * 'name' - The name of the facet to delete.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
mkDeleteFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeleteFacet
mkDeleteFacet pSchemaARN_ pName_ =
  DeleteFacet' {schemaARN = pSchemaARN_, name = pName_}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfSchemaARN :: Lens.Lens' DeleteFacet Lude.Text
dfSchemaARN = Lens.lens (schemaARN :: DeleteFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: DeleteFacet)
{-# DEPRECATED dfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the facet to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' DeleteFacet Lude.Text
dfName = Lens.lens (name :: DeleteFacet -> Lude.Text) (\s a -> s {name = a} :: DeleteFacet)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteFacet where
  type Rs DeleteFacet = DeleteFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFacetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFacet where
  toHeaders DeleteFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON DeleteFacet where
  toJSON DeleteFacet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteFacet where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/facet/delete"

instance Lude.ToQuery DeleteFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFacetResponse' smart constructor.
newtype DeleteFacetResponse = DeleteFacetResponse'
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

-- | Creates a value of 'DeleteFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFacetResponse
mkDeleteFacetResponse pResponseStatus_ =
  DeleteFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DeleteFacetResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DeleteFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFacetResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
