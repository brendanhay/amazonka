{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use 'DeleteAttributes' . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes Attributes> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.PutAttributes
  ( -- * Creating a request
    PutAttributes (..),
    mkPutAttributes,

    -- ** Request lenses
    paCluster,
    paAttributes,

    -- * Destructuring the response
    PutAttributesResponse (..),
    mkPutAttributesResponse,

    -- ** Response lenses
    parsAttributes,
    parsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAttributes' smart constructor.
data PutAttributes = PutAttributes'
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

-- | Creates a value of 'PutAttributes' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
mkPutAttributes ::
  PutAttributes
mkPutAttributes =
  PutAttributes' {cluster = Lude.Nothing, attributes = Lude.mempty}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that contains the resource to apply attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paCluster :: Lens.Lens' PutAttributes (Lude.Maybe Lude.Text)
paCluster = Lens.lens (cluster :: PutAttributes -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: PutAttributes)
{-# DEPRECATED paCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The attributes to apply to your resource. You can specify up to 10 custom attributes per resource. You can specify up to 10 attributes in a single call.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PutAttributes [Attribute]
paAttributes = Lens.lens (attributes :: PutAttributes -> [Attribute]) (\s a -> s {attributes = a} :: PutAttributes)
{-# DEPRECATED paAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest PutAttributes where
  type Rs PutAttributes = PutAttributesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAttributesResponse'
            Lude.<$> (x Lude..?> "attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.PutAttributes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAttributes where
  toJSON PutAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )

instance Lude.ToPath PutAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAttributesResponse' smart constructor.
data PutAttributesResponse = PutAttributesResponse'
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

-- | Creates a value of 'PutAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes applied to your resource.
-- * 'responseStatus' - The response status code.
mkPutAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAttributesResponse
mkPutAttributesResponse pResponseStatus_ =
  PutAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attributes applied to your resource.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsAttributes :: Lens.Lens' PutAttributesResponse (Lude.Maybe [Attribute])
parsAttributes = Lens.lens (attributes :: PutAttributesResponse -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: PutAttributesResponse)
{-# DEPRECATED parsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parsResponseStatus :: Lens.Lens' PutAttributesResponse Lude.Int
parsResponseStatus = Lens.lens (responseStatus :: PutAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAttributesResponse)
{-# DEPRECATED parsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
