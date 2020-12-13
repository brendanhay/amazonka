{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.GetNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a namespace.
module Network.AWS.Route53AutoNaming.GetNamespace
  ( -- * Creating a request
    GetNamespace (..),
    mkGetNamespace,

    -- ** Request lenses
    gnId,

    -- * Destructuring the response
    GetNamespaceResponse (..),
    mkGetNamespaceResponse,

    -- ** Response lenses
    gnrsNamespace,
    gnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkGetNamespace' smart constructor.
newtype GetNamespace = GetNamespace'
  { -- | The ID of the namespace that you want to get information about.
    id :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNamespace' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the namespace that you want to get information about.
mkGetNamespace ::
  -- | 'id'
  Lude.Text ->
  GetNamespace
mkGetNamespace pId_ = GetNamespace' {id = pId_}

-- | The ID of the namespace that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnId :: Lens.Lens' GetNamespace Lude.Text
gnId = Lens.lens (id :: GetNamespace -> Lude.Text) (\s a -> s {id = a} :: GetNamespace)
{-# DEPRECATED gnId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.AWSRequest GetNamespace where
  type Rs GetNamespace = GetNamespaceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetNamespaceResponse'
            Lude.<$> (x Lude..?> "Namespace") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetNamespace where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.GetNamespace" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetNamespace where
  toJSON GetNamespace' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Id" Lude..= id)])

instance Lude.ToPath GetNamespace where
  toPath = Lude.const "/"

instance Lude.ToQuery GetNamespace where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetNamespaceResponse' smart constructor.
data GetNamespaceResponse = GetNamespaceResponse'
  { -- | A complex type that contains information about the specified namespace.
    namespace :: Lude.Maybe Namespace,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetNamespaceResponse' with the minimum fields required to make a request.
--
-- * 'namespace' - A complex type that contains information about the specified namespace.
-- * 'responseStatus' - The response status code.
mkGetNamespaceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetNamespaceResponse
mkGetNamespaceResponse pResponseStatus_ =
  GetNamespaceResponse'
    { namespace = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains information about the specified namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnrsNamespace :: Lens.Lens' GetNamespaceResponse (Lude.Maybe Namespace)
gnrsNamespace = Lens.lens (namespace :: GetNamespaceResponse -> Lude.Maybe Namespace) (\s a -> s {namespace = a} :: GetNamespaceResponse)
{-# DEPRECATED gnrsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gnrsResponseStatus :: Lens.Lens' GetNamespaceResponse Lude.Int
gnrsResponseStatus = Lens.lens (responseStatus :: GetNamespaceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetNamespaceResponse)
{-# DEPRECATED gnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
