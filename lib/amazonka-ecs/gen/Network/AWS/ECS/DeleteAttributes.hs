{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DeleteAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more custom attributes from an Amazon ECS resource.
module Network.AWS.ECS.DeleteAttributes
  ( -- * Creating a request
    DeleteAttributes (..),
    mkDeleteAttributes,

    -- ** Request lenses
    daCluster,
    daAttributes,

    -- * Destructuring the response
    DeleteAttributesResponse (..),
    mkDeleteAttributesResponse,

    -- ** Response lenses
    darsAttributes,
    darsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { cluster ::
      Lude.Maybe Lude.Text,
    attributes :: [Attribute]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
mkDeleteAttributes ::
  DeleteAttributes
mkDeleteAttributes =
  DeleteAttributes'
    { cluster = Lude.Nothing,
      attributes = Lude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to delete attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daCluster :: Lens.Lens' DeleteAttributes (Lude.Maybe Lude.Text)
daCluster = Lens.lens (cluster :: DeleteAttributes -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DeleteAttributes)
{-# DEPRECATED daCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The attributes to delete from your resource. You can specify up to 10 attributes per request. For custom attributes, specify the attribute name and target ID, but do not specify the value. If you specify the target ID using the short form, you must also specify the target type.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttributes :: Lens.Lens' DeleteAttributes [Attribute]
daAttributes = Lens.lens (attributes :: DeleteAttributes -> [Attribute]) (\s a -> s {attributes = a} :: DeleteAttributes)
{-# DEPRECATED daAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest DeleteAttributes where
  type Rs DeleteAttributes = DeleteAttributesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAttributesResponse'
            Lude.<$> (x Lude..?> "attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DeleteAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAttributes where
  toJSON DeleteAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )

instance Lude.ToPath DeleteAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse'
  { attributes ::
      Lude.Maybe [Attribute],
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

-- | Creates a value of 'DeleteAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - A list of attribute objects that were successfully deleted from your resource.
-- * 'responseStatus' - The response status code.
mkDeleteAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAttributesResponse
mkDeleteAttributesResponse pResponseStatus_ =
  DeleteAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of attribute objects that were successfully deleted from your resource.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAttributes :: Lens.Lens' DeleteAttributesResponse (Lude.Maybe [Attribute])
darsAttributes = Lens.lens (attributes :: DeleteAttributesResponse -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: DeleteAttributesResponse)
{-# DEPRECATED darsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteAttributesResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAttributesResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
