{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private namespace based on DNS, which will be visible only inside a specified Amazon VPC. The namespace defines your service naming scheme. For example, if you name your namespace @example.com@ and name your service @backend@ , the resulting DNS name for the service will be @backend.example.com@ . For the current quota on the number of namespaces that you can create using the same AWS account, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreatePrivateDNSNamespace
  ( -- * Creating a request
    CreatePrivateDNSNamespace (..),
    mkCreatePrivateDNSNamespace,

    -- ** Request lenses
    cpdnCreatorRequestId,
    cpdnName,
    cpdnVPC,
    cpdnDescription,
    cpdnTags,

    -- * Destructuring the response
    CreatePrivateDNSNamespaceResponse (..),
    mkCreatePrivateDNSNamespaceResponse,

    -- ** Response lenses
    cpdnsnrsOperationId,
    cpdnsnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkCreatePrivateDNSNamespace' smart constructor.
data CreatePrivateDNSNamespace = CreatePrivateDNSNamespace'
  { -- | A unique string that identifies the request and that allows failed @CreatePrivateDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
    creatorRequestId :: Lude.Maybe Lude.Text,
    -- | The name that you want to assign to this namespace. When you create a private DNS namespace, AWS Cloud Map automatically creates an Amazon Route 53 private hosted zone that has the same name as the namespace.
    name :: Lude.Text,
    -- | The ID of the Amazon VPC that you want to associate the namespace with.
    vpc :: Lude.Text,
    -- | A description for the namespace.
    description :: Lude.Maybe Lude.Text,
    -- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePrivateDNSNamespace' with the minimum fields required to make a request.
--
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed @CreatePrivateDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
-- * 'name' - The name that you want to assign to this namespace. When you create a private DNS namespace, AWS Cloud Map automatically creates an Amazon Route 53 private hosted zone that has the same name as the namespace.
-- * 'vpc' - The ID of the Amazon VPC that you want to associate the namespace with.
-- * 'description' - A description for the namespace.
-- * 'tags' - The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
mkCreatePrivateDNSNamespace ::
  -- | 'name'
  Lude.Text ->
  -- | 'vpc'
  Lude.Text ->
  CreatePrivateDNSNamespace
mkCreatePrivateDNSNamespace pName_ pVPC_ =
  CreatePrivateDNSNamespace'
    { creatorRequestId = Lude.Nothing,
      name = pName_,
      vpc = pVPC_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A unique string that identifies the request and that allows failed @CreatePrivateDnsNamespace@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnCreatorRequestId :: Lens.Lens' CreatePrivateDNSNamespace (Lude.Maybe Lude.Text)
cpdnCreatorRequestId = Lens.lens (creatorRequestId :: CreatePrivateDNSNamespace -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: CreatePrivateDNSNamespace)
{-# DEPRECATED cpdnCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | The name that you want to assign to this namespace. When you create a private DNS namespace, AWS Cloud Map automatically creates an Amazon Route 53 private hosted zone that has the same name as the namespace.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnName :: Lens.Lens' CreatePrivateDNSNamespace Lude.Text
cpdnName = Lens.lens (name :: CreatePrivateDNSNamespace -> Lude.Text) (\s a -> s {name = a} :: CreatePrivateDNSNamespace)
{-# DEPRECATED cpdnName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the Amazon VPC that you want to associate the namespace with.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnVPC :: Lens.Lens' CreatePrivateDNSNamespace Lude.Text
cpdnVPC = Lens.lens (vpc :: CreatePrivateDNSNamespace -> Lude.Text) (\s a -> s {vpc = a} :: CreatePrivateDNSNamespace)
{-# DEPRECATED cpdnVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | A description for the namespace.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnDescription :: Lens.Lens' CreatePrivateDNSNamespace (Lude.Maybe Lude.Text)
cpdnDescription = Lens.lens (description :: CreatePrivateDNSNamespace -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePrivateDNSNamespace)
{-# DEPRECATED cpdnDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to add to the namespace. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnTags :: Lens.Lens' CreatePrivateDNSNamespace (Lude.Maybe [Tag])
cpdnTags = Lens.lens (tags :: CreatePrivateDNSNamespace -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePrivateDNSNamespace)
{-# DEPRECATED cpdnTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePrivateDNSNamespace where
  type
    Rs CreatePrivateDNSNamespace =
      CreatePrivateDNSNamespaceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePrivateDNSNamespaceResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePrivateDNSNamespace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53AutoNaming_v20170314.CreatePrivateDnsNamespace" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePrivateDNSNamespace where
  toJSON CreatePrivateDNSNamespace' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CreatorRequestId" Lude..=) Lude.<$> creatorRequestId,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Vpc" Lude..= vpc),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePrivateDNSNamespace where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePrivateDNSNamespace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePrivateDNSNamespaceResponse' smart constructor.
data CreatePrivateDNSNamespaceResponse = CreatePrivateDNSNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePrivateDNSNamespaceResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
-- * 'responseStatus' - The response status code.
mkCreatePrivateDNSNamespaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePrivateDNSNamespaceResponse
mkCreatePrivateDNSNamespaceResponse pResponseStatus_ =
  CreatePrivateDNSNamespaceResponse'
    { operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A value that you can use to determine whether the request completed successfully. To get the status of the operation, see <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnrsOperationId :: Lens.Lens' CreatePrivateDNSNamespaceResponse (Lude.Maybe Lude.Text)
cpdnsnrsOperationId = Lens.lens (operationId :: CreatePrivateDNSNamespaceResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CreatePrivateDNSNamespaceResponse)
{-# DEPRECATED cpdnsnrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpdnsnrsResponseStatus :: Lens.Lens' CreatePrivateDNSNamespaceResponse Lude.Int
cpdnsnrsResponseStatus = Lens.lens (responseStatus :: CreatePrivateDNSNamespaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePrivateDNSNamespaceResponse)
{-# DEPRECATED cpdnsnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
