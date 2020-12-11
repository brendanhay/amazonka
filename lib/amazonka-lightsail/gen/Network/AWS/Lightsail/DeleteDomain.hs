{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain recordset and all of its domain records.
--
-- The @delete domain@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDomain
  ( -- * Creating a request
    DeleteDomain (..),
    mkDeleteDomain,

    -- ** Request lenses
    ddDomainName,

    -- * Destructuring the response
    DeleteDomainResponse (..),
    mkDeleteDomainResponse,

    -- ** Response lenses
    delrsOperation,
    delrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDomain' smart constructor.
newtype DeleteDomain = DeleteDomain' {domainName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The specific domain name to delete.
mkDeleteDomain ::
  -- | 'domainName'
  Lude.Text ->
  DeleteDomain
mkDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | The specific domain name to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DeleteDomain Lude.Text
ddDomainName = Lens.lens (domainName :: DeleteDomain -> Lude.Text) (\s a -> s {domainName = a} :: DeleteDomain)
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDomainResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("domainName" Lude..= domainName)])

instance Lude.ToPath DeleteDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
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

-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDomainResponse
mkDeleteDomainResponse pResponseStatus_ =
  DeleteDomainResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsOperation :: Lens.Lens' DeleteDomainResponse (Lude.Maybe Operation)
delrsOperation = Lens.lens (operation :: DeleteDomainResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: DeleteDomainResponse)
{-# DEPRECATED delrsOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delrsResponseStatus :: Lens.Lens' DeleteDomainResponse Lude.Int
delrsResponseStatus = Lens.lens (responseStatus :: DeleteDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDomainResponse)
{-# DEPRECATED delrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
