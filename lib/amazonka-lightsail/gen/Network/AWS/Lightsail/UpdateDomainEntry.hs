{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UpdateDomainEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a domain recordset after it is created.
--
-- The @update domain entry@ operation supports tag-based access control via resource tags applied to the resource identified by @domain name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.UpdateDomainEntry
  ( -- * Creating a request
    UpdateDomainEntry (..),
    mkUpdateDomainEntry,

    -- ** Request lenses
    udeDomainEntry,
    udeDomainName,

    -- * Destructuring the response
    UpdateDomainEntryResponse (..),
    mkUpdateDomainEntryResponse,

    -- ** Response lenses
    udersOperations,
    udersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDomainEntry' smart constructor.
data UpdateDomainEntry = UpdateDomainEntry'
  { -- | An array of key-value pairs containing information about the domain entry.
    domainEntry :: DomainEntry,
    -- | The name of the domain recordset to update.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainEntry' with the minimum fields required to make a request.
--
-- * 'domainEntry' - An array of key-value pairs containing information about the domain entry.
-- * 'domainName' - The name of the domain recordset to update.
mkUpdateDomainEntry ::
  -- | 'domainEntry'
  DomainEntry ->
  -- | 'domainName'
  Lude.Text ->
  UpdateDomainEntry
mkUpdateDomainEntry pDomainEntry_ pDomainName_ =
  UpdateDomainEntry'
    { domainEntry = pDomainEntry_,
      domainName = pDomainName_
    }

-- | An array of key-value pairs containing information about the domain entry.
--
-- /Note:/ Consider using 'domainEntry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDomainEntry :: Lens.Lens' UpdateDomainEntry DomainEntry
udeDomainEntry = Lens.lens (domainEntry :: UpdateDomainEntry -> DomainEntry) (\s a -> s {domainEntry = a} :: UpdateDomainEntry)
{-# DEPRECATED udeDomainEntry "Use generic-lens or generic-optics with 'domainEntry' instead." #-}

-- | The name of the domain recordset to update.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udeDomainName :: Lens.Lens' UpdateDomainEntry Lude.Text
udeDomainName = Lens.lens (domainName :: UpdateDomainEntry -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainEntry)
{-# DEPRECATED udeDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest UpdateDomainEntry where
  type Rs UpdateDomainEntry = UpdateDomainEntryResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainEntryResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDomainEntry where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.UpdateDomainEntry" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDomainEntry where
  toJSON UpdateDomainEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domainEntry" Lude..= domainEntry),
            Lude.Just ("domainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath UpdateDomainEntry where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomainEntry where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDomainEntryResponse' smart constructor.
data UpdateDomainEntryResponse = UpdateDomainEntryResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainEntryResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkUpdateDomainEntryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDomainEntryResponse
mkUpdateDomainEntryResponse pResponseStatus_ =
  UpdateDomainEntryResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udersOperations :: Lens.Lens' UpdateDomainEntryResponse (Lude.Maybe [Operation])
udersOperations = Lens.lens (operations :: UpdateDomainEntryResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: UpdateDomainEntryResponse)
{-# DEPRECATED udersOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udersResponseStatus :: Lens.Lens' UpdateDomainEntryResponse Lude.Int
udersResponseStatus = Lens.lens (responseStatus :: UpdateDomainEntryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainEntryResponse)
{-# DEPRECATED udersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
