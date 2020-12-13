{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public namespace based on DNS, which will be visible on the internet. The namespace defines your service naming scheme. For example, if you name your namespace @example.com@ and name your service @backend@ , the resulting DNS name for the service will be @backend.example.com@ . For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreatePublicDNSNamespace
  ( -- * Creating a request
    CreatePublicDNSNamespace (..),
    mkCreatePublicDNSNamespace,

    -- ** Request lenses
    cpdnsnCreatorRequestId,
    cpdnsnName,
    cpdnsnDescription,
    cpdnsnTags,

    -- * Destructuring the response
    CreatePublicDNSNamespaceResponse (..),
    mkCreatePublicDNSNamespaceResponse,

    -- ** Response lenses
    cpdnrsOperationId,
    cpdnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkCreatePublicDNSNamespace' smart constructor.
data CreatePublicDNSNamespace = CreatePublicDNSNamespace'
  { -- | A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
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

-- | Creates a value of 'CreatePublicDNSNamespace' with the minimum fields required to make a request.
--
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
-- * 'name' - The name that you want to assign to this namespace.
-- * 'description' - A description for the namespace.
-- * 'tags' - The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
mkCreatePublicDNSNamespace ::
  -- | 'name'
  Lude.Text ->
  CreatePublicDNSNamespace
mkCreatePublicDNSNamespace pName_ =
  CreatePublicDNSNamespace'
    { creatorRequestId = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A unique string that identifies the request and that allows failed @CreatePublicDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnCreatorRequestId :: Lens.Lens' CreatePublicDNSNamespace (Lude.Maybe Lude.Text)
cpdnsnCreatorRequestId = Lens.lens (creatorRequestId :: CreatePublicDNSNamespace -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: CreatePublicDNSNamespace)
{-# DEPRECATED cpdnsnCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The name that you want to assign to this namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnName :: Lens.Lens' CreatePublicDNSNamespace Lude.Text
cpdnsnName = Lens.lens (name :: CreatePublicDNSNamespace -> Lude.Text) (\s a -> s {name = a} :: CreatePublicDNSNamespace)
{-# DEPRECATED cpdnsnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnDescription :: Lens.Lens' CreatePublicDNSNamespace (Lude.Maybe Lude.Text)
cpdnsnDescription = Lens.lens (description :: CreatePublicDNSNamespace -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePublicDNSNamespace)
{-# DEPRECATED cpdnsnDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnTags :: Lens.Lens' CreatePublicDNSNamespace (Lude.Maybe [Tag])
cpdnsnTags = Lens.lens (tags :: CreatePublicDNSNamespace -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePublicDNSNamespace)
{-# DEPRECATED cpdnsnTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePublicDNSNamespace where
  type Rs CreatePublicDNSNamespace = CreatePublicDNSNamespaceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePublicDNSNamespaceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePublicDNSNamespace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.CreatePublicDnsNamespace" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePublicDNSNamespace where
  toJSON CreatePublicDNSNamespace' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatorRequestId" Lude..=) Lude.<$> creatorRequestId,
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePublicDNSNamespace where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePublicDNSNamespace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePublicDNSNamespaceResponse' smart constructor.
data CreatePublicDNSNamespaceResponse = CreatePublicDNSNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePublicDNSNamespaceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkCreatePublicDNSNamespaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePublicDNSNamespaceResponse
mkCreatePublicDNSNamespaceResponse pResponseStatus_ =
  CreatePublicDNSNamespaceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnrsOperationId :: Lens.Lens' CreatePublicDNSNamespaceResponse (Lude.Maybe Lude.Text)
cpdnrsOperationId = Lens.lens (operationId :: CreatePublicDNSNamespaceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CreatePublicDNSNamespaceResponse)
{-# DEPRECATED cpdnrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnrsResponseStatus :: Lens.Lens' CreatePublicDNSNamespaceResponse Lude.Int
cpdnrsResponseStatus = Lens.lens (responseStatus :: CreatePublicDNSNamespaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePublicDNSNamespaceResponse)
{-# DEPRECATED cpdnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
