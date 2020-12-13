{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreateHTTPNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HTTP namespace. Service instances that you register using an HTTP namespace can be discovered using a @DiscoverInstances@ request but can't be discovered using DNS.
--
-- For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map quotas> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreateHTTPNamespace
  ( -- * Creating a request
    CreateHTTPNamespace (..),
    mkCreateHTTPNamespace,

    -- ** Request lenses
    chttpnCreatorRequestId,
    chttpnName,
    chttpnDescription,
    chttpnTags,

    -- * Destructuring the response
    CreateHTTPNamespaceResponse (..),
    mkCreateHTTPNamespaceResponse,

    -- ** Response lenses
    chttpnrsOperationId,
    chttpnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkCreateHTTPNamespace' smart constructor.
data CreateHTTPNamespace = CreateHTTPNamespace'
  { -- | A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
    creatorRequestId :: Lude.Maybe Lude.Text,
    -- | The name that you want to assign to this namespace.
    name :: Lude.Text,
    -- | A description for the namespace.
    description :: Lude.Maybe Lude.Text,
    -- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHTTPNamespace' with the minimum fields required to make a request.
--
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
-- * 'name' - The name that you want to assign to this namespace.
-- * 'description' - A description for the namespace.
-- * 'tags' - The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
mkCreateHTTPNamespace ::
  -- | 'name'
  Lude.Text ->
  CreateHTTPNamespace
mkCreateHTTPNamespace pName_ =
  CreateHTTPNamespace'
    { creatorRequestId = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A unique string that identifies the request and that allows failed @CreateHttpNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnCreatorRequestId :: Lens.Lens' CreateHTTPNamespace (Lude.Maybe Lude.Text)
chttpnCreatorRequestId = Lens.lens (creatorRequestId :: CreateHTTPNamespace -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: CreateHTTPNamespace)
{-# DEPRECATED chttpnCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The name that you want to assign to this namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnName :: Lens.Lens' CreateHTTPNamespace Lude.Text
chttpnName = Lens.lens (name :: CreateHTTPNamespace -> Lude.Text) (\s a -> s {name = a} :: CreateHTTPNamespace)
{-# DEPRECATED chttpnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnDescription :: Lens.Lens' CreateHTTPNamespace (Lude.Maybe Lude.Text)
chttpnDescription = Lens.lens (description :: CreateHTTPNamespace -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateHTTPNamespace)
{-# DEPRECATED chttpnDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnTags :: Lens.Lens' CreateHTTPNamespace (Lude.Maybe [Tag])
chttpnTags = Lens.lens (tags :: CreateHTTPNamespace -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateHTTPNamespace)
{-# DEPRECATED chttpnTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateHTTPNamespace where
  type Rs CreateHTTPNamespace = CreateHTTPNamespaceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateHTTPNamespaceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateHTTPNamespace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.CreateHttpNamespace" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateHTTPNamespace where
  toJSON CreateHTTPNamespace' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatorRequestId" Lude..=) Lude.<$> creatorRequestId,
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateHTTPNamespace where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateHTTPNamespace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateHTTPNamespaceResponse' smart constructor.
data CreateHTTPNamespaceResponse = CreateHTTPNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateHTTPNamespaceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkCreateHTTPNamespaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateHTTPNamespaceResponse
mkCreateHTTPNamespaceResponse pResponseStatus_ =
  CreateHTTPNamespaceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnrsOperationId :: Lens.Lens' CreateHTTPNamespaceResponse (Lude.Maybe Lude.Text)
chttpnrsOperationId = Lens.lens (operationId :: CreateHTTPNamespaceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CreateHTTPNamespaceResponse)
{-# DEPRECATED chttpnrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chttpnrsResponseStatus :: Lens.Lens' CreateHTTPNamespaceResponse Lude.Int
chttpnrsResponseStatus = Lens.lens (responseStatus :: CreateHTTPNamespaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateHTTPNamespaceResponse)
{-# DEPRECATED chttpnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
