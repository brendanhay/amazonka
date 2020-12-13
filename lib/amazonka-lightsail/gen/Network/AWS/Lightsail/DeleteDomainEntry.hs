{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific domain entry.
--
-- The @delete domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDomainEntry
  ( -- * Creating a request
    DeleteDomainEntry (..),
    mkDeleteDomainEntry,

    -- ** Request lenses
    ddeDomainEntry,
    ddeDomainName,

    -- * Destructuring the response
    DeleteDomainEntryResponse (..),
    mkDeleteDomainEntryResponse,

    -- ** Response lenses
    ddersOperation,
    ddersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDomainEntry' smart constructor.
data DeleteDomainEntry = DeleteDomainEntry'
  { -- | An array of key-value pairs containing information about your domain entries.
    domainEntry :: DomainEntry,
    -- | The name of the domain entry to delete.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainEntry' with the minimum fields required to make a request.
--
-- * 'domainEntry' - An array of key-value pairs containing information about your domain entries.
-- * 'domainName' - The name of the domain entry to delete.
mkDeleteDomainEntry ::
  -- | 'domainEntry'
  DomainEntry ->
  -- | 'domainName'
  Lude.Text ->
  DeleteDomainEntry
mkDeleteDomainEntry pDomainEntry_ pDomainName_ =
  DeleteDomainEntry'
    { domainEntry = pDomainEntry_,
      domainName = pDomainName_
    }

-- | An array of key-value pairs containing information about your domain entries.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainEntry :: Lens.Lens' DeleteDomainEntry DomainEntry
ddeDomainEntry = Lens.lens (domainEntry :: DeleteDomainEntry -> DomainEntry) (\s a -> s {domainEntry = a} :: DeleteDomainEntry)
{-# DEPRECATED ddeDomainEntry "Use generic-lens or generic-optics with 'domainEntry' instead." #-}

-- | The name of the domain entry to delete.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddeDomainName :: Lens.Lens' DeleteDomainEntry Lude.Text
ddeDomainName = Lens.lens (domainName :: DeleteDomainEntry -> Lude.Text) (\s a -> s {domainName = a} :: DeleteDomainEntry)
{-# DEPRECATED ddeDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DeleteDomainEntry where
  type Rs DeleteDomainEntry = DeleteDomainEntryResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDomainEntryResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDomainEntry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteDomainEntry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDomainEntry where
  toJSON DeleteDomainEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domainEntry" Lude..= domainEntry),
            Lude.Just ("domainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath DeleteDomainEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDomainEntry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDomainEntryResponse' smart constructor.
data DeleteDomainEntryResponse = DeleteDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDomainEntryResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteDomainEntryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDomainEntryResponse
mkDeleteDomainEntryResponse pResponseStatus_ =
  DeleteDomainEntryResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddersOperation :: Lens.Lens' DeleteDomainEntryResponse (Lude.Maybe Operation)
ddersOperation = Lens.lens (operation :: DeleteDomainEntryResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: DeleteDomainEntryResponse)
{-# DEPRECATED ddersOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddersResponseStatus :: Lens.Lens' DeleteDomainEntryResponse Lude.Int
ddersResponseStatus = Lens.lens (responseStatus :: DeleteDomainEntryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDomainEntryResponse)
{-# DEPRECATED ddersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
