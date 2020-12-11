{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DeleteTypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.DeleteTypedLinkFacet
  ( -- * Creating a request
    DeleteTypedLinkFacet (..),
    mkDeleteTypedLinkFacet,

    -- ** Request lenses
    dtlfSchemaARN,
    dtlfName,

    -- * Destructuring the response
    DeleteTypedLinkFacetResponse (..),
    mkDeleteTypedLinkFacetResponse,

    -- ** Response lenses
    dtlfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTypedLinkFacet' smart constructor.
data DeleteTypedLinkFacet = DeleteTypedLinkFacet'
  { schemaARN ::
      Lude.Text,
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

-- | Creates a value of 'DeleteTypedLinkFacet' with the minimum fields required to make a request.
--
-- * 'name' - The unique name of the typed link facet.
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
mkDeleteTypedLinkFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  DeleteTypedLinkFacet
mkDeleteTypedLinkFacet pSchemaARN_ pName_ =
  DeleteTypedLinkFacet' {schemaARN = pSchemaARN_, name = pName_}

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlfSchemaARN :: Lens.Lens' DeleteTypedLinkFacet Lude.Text
dtlfSchemaARN = Lens.lens (schemaARN :: DeleteTypedLinkFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: DeleteTypedLinkFacet)
{-# DEPRECATED dtlfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlfName :: Lens.Lens' DeleteTypedLinkFacet Lude.Text
dtlfName = Lens.lens (name :: DeleteTypedLinkFacet -> Lude.Text) (\s a -> s {name = a} :: DeleteTypedLinkFacet)
{-# DEPRECATED dtlfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteTypedLinkFacet where
  type Rs DeleteTypedLinkFacet = DeleteTypedLinkFacetResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTypedLinkFacetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTypedLinkFacet where
  toHeaders DeleteTypedLinkFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON DeleteTypedLinkFacet where
  toJSON DeleteTypedLinkFacet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteTypedLinkFacet where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/delete"

instance Lude.ToQuery DeleteTypedLinkFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTypedLinkFacetResponse' smart constructor.
newtype DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse'
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

-- | Creates a value of 'DeleteTypedLinkFacetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTypedLinkFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTypedLinkFacetResponse
mkDeleteTypedLinkFacetResponse pResponseStatus_ =
  DeleteTypedLinkFacetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlfrsResponseStatus :: Lens.Lens' DeleteTypedLinkFacetResponse Lude.Int
dtlfrsResponseStatus = Lens.lens (responseStatus :: DeleteTypedLinkFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTypedLinkFacetResponse)
{-# DEPRECATED dtlfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
