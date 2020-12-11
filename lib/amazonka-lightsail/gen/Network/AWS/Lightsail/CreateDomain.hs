{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain resource for the specified domain (e.g., example.com).
--
-- The @create domain@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDomain
  ( -- * Creating a request
    CreateDomain (..),
    mkCreateDomain,

    -- ** Request lenses
    creTags,
    creDomainName,

    -- * Destructuring the response
    CreateDomainResponse (..),
    mkCreateDomainResponse,

    -- ** Response lenses
    cdrsOperation,
    cdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { tags :: Lude.Maybe [Tag],
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain name to manage (e.g., @example.com@ ).
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateDomain ::
  -- | 'domainName'
  Lude.Text ->
  CreateDomain
mkCreateDomain pDomainName_ =
  CreateDomain' {tags = Lude.Nothing, domainName = pDomainName_}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creTags :: Lens.Lens' CreateDomain (Lude.Maybe [Tag])
creTags = Lens.lens (tags :: CreateDomain -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDomain)
{-# DEPRECATED creTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The domain name to manage (e.g., @example.com@ ).
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
creDomainName :: Lens.Lens' CreateDomain Lude.Text
creDomainName = Lens.lens (domainName :: CreateDomain -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomain)
{-# DEPRECATED creDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CreateDomain where
  type Rs CreateDomain = CreateDomainResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("domainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath CreateDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { operation ::
      Lude.Maybe Operation,
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

-- | Creates a value of 'CreateDomainResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDomainResponse
mkCreateDomainResponse pResponseStatus_ =
  CreateDomainResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsOperation :: Lens.Lens' CreateDomainResponse (Lude.Maybe Operation)
cdrsOperation = Lens.lens (operation :: CreateDomainResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrsResponseStatus :: Lens.Lens' CreateDomainResponse Lude.Int
cdrsResponseStatus = Lens.lens (responseStatus :: CreateDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDomainResponse)
{-# DEPRECATED cdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
