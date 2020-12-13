{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one of the following domain name system (DNS) records in a domain DNS zone: Address (A), canonical name (CNAME), mail exchanger (MX), name server (NS), start of authority (SOA), service locator (SRV), or text (TXT).
--
-- The @create domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDomainEntry
  ( -- * Creating a request
    CreateDomainEntry (..),
    mkCreateDomainEntry,

    -- ** Request lenses
    cdeDomainEntry,
    cdeDomainName,

    -- * Destructuring the response
    CreateDomainEntryResponse (..),
    mkCreateDomainEntryResponse,

    -- ** Response lenses
    cdersOperation,
    cdersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDomainEntry' smart constructor.
data CreateDomainEntry = CreateDomainEntry'
  { -- | An array of key-value pairs containing information about the domain entry request.
    domainEntry :: DomainEntry,
    -- | The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainEntry' with the minimum fields required to make a request.
--
-- * 'domainEntry' - An array of key-value pairs containing information about the domain entry request.
-- * 'domainName' - The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
mkCreateDomainEntry ::
  -- | 'domainEntry'
  DomainEntry ->
  -- | 'domainName'
  Lude.Text ->
  CreateDomainEntry
mkCreateDomainEntry pDomainEntry_ pDomainName_ =
  CreateDomainEntry'
    { domainEntry = pDomainEntry_,
      domainName = pDomainName_
    }

-- | An array of key-value pairs containing information about the domain entry request.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeDomainEntry :: Lens.Lens' CreateDomainEntry DomainEntry
cdeDomainEntry = Lens.lens (domainEntry :: CreateDomainEntry -> DomainEntry) (\s a -> s {domainEntry = a} :: CreateDomainEntry)
{-# DEPRECATED cdeDomainEntry "Use generic-lens or generic-optics with 'domainEntry' instead." #-}

-- | The domain name (e.g., @example.com@ ) for which you want to create the domain entry.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdeDomainName :: Lens.Lens' CreateDomainEntry Lude.Text
cdeDomainName = Lens.lens (domainName :: CreateDomainEntry -> Lude.Text) (\s a -> s {domainName = a} :: CreateDomainEntry)
{-# DEPRECATED cdeDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CreateDomainEntry where
  type Rs CreateDomainEntry = CreateDomainEntryResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDomainEntryResponse'
            Lude.<$> (x Lude..?> "operation") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDomainEntry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateDomainEntry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDomainEntry where
  toJSON CreateDomainEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domainEntry" Lude..= domainEntry),
            Lude.Just ("domainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath CreateDomainEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDomainEntry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDomainEntryResponse' smart constructor.
data CreateDomainEntryResponse = CreateDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operation :: Lude.Maybe Operation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDomainEntryResponse' with the minimum fields required to make a request.
--
-- * 'operation' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateDomainEntryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDomainEntryResponse
mkCreateDomainEntryResponse pResponseStatus_ =
  CreateDomainEntryResponse'
    { operation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersOperation :: Lens.Lens' CreateDomainEntryResponse (Lude.Maybe Operation)
cdersOperation = Lens.lens (operation :: CreateDomainEntryResponse -> Lude.Maybe Operation) (\s a -> s {operation = a} :: CreateDomainEntryResponse)
{-# DEPRECATED cdersOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdersResponseStatus :: Lens.Lens' CreateDomainEntryResponse Lude.Int
cdersResponseStatus = Lens.lens (responseStatus :: CreateDomainEntryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDomainEntryResponse)
{-# DEPRECATED cdersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
