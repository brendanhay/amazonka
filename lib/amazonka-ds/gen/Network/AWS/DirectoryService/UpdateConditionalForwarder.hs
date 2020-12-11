{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a conditional forwarder that has been set up for your AWS directory.
module Network.AWS.DirectoryService.UpdateConditionalForwarder
  ( -- * Creating a request
    UpdateConditionalForwarder (..),
    mkUpdateConditionalForwarder,

    -- ** Request lenses
    ucfDirectoryId,
    ucfRemoteDomainName,
    ucfDNSIPAddrs,

    -- * Destructuring the response
    UpdateConditionalForwarderResponse (..),
    mkUpdateConditionalForwarderResponse,

    -- ** Response lenses
    ucfrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Updates a conditional forwarder.
--
-- /See:/ 'mkUpdateConditionalForwarder' smart constructor.
data UpdateConditionalForwarder = UpdateConditionalForwarder'
  { directoryId ::
      Lude.Text,
    remoteDomainName :: Lude.Text,
    dnsIPAddrs :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConditionalForwarder' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory ID of the AWS directory for which to update the conditional forwarder.
-- * 'dnsIPAddrs' - The updated IP addresses of the remote DNS server associated with the conditional forwarder.
-- * 'remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
mkUpdateConditionalForwarder ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'remoteDomainName'
  Lude.Text ->
  UpdateConditionalForwarder
mkUpdateConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  UpdateConditionalForwarder'
    { directoryId = pDirectoryId_,
      remoteDomainName = pRemoteDomainName_,
      dnsIPAddrs = Lude.mempty
    }

-- | The directory ID of the AWS directory for which to update the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDirectoryId :: Lens.Lens' UpdateConditionalForwarder Lude.Text
ucfDirectoryId = Lens.lens (directoryId :: UpdateConditionalForwarder -> Lude.Text) (\s a -> s {directoryId = a} :: UpdateConditionalForwarder)
{-# DEPRECATED ucfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfRemoteDomainName :: Lens.Lens' UpdateConditionalForwarder Lude.Text
ucfRemoteDomainName = Lens.lens (remoteDomainName :: UpdateConditionalForwarder -> Lude.Text) (\s a -> s {remoteDomainName = a} :: UpdateConditionalForwarder)
{-# DEPRECATED ucfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The updated IP addresses of the remote DNS server associated with the conditional forwarder.
--
-- /Note:/ Consider using 'dnsIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfDNSIPAddrs :: Lens.Lens' UpdateConditionalForwarder [Lude.Text]
ucfDNSIPAddrs = Lens.lens (dnsIPAddrs :: UpdateConditionalForwarder -> [Lude.Text]) (\s a -> s {dnsIPAddrs = a} :: UpdateConditionalForwarder)
{-# DEPRECATED ucfDNSIPAddrs "Use generic-lens or generic-optics with 'dnsIPAddrs' instead." #-}

instance Lude.AWSRequest UpdateConditionalForwarder where
  type
    Rs UpdateConditionalForwarder =
      UpdateConditionalForwarderResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateConditionalForwarderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateConditionalForwarder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.UpdateConditionalForwarder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateConditionalForwarder where
  toJSON UpdateConditionalForwarder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RemoteDomainName" Lude..= remoteDomainName),
            Lude.Just ("DnsIpAddrs" Lude..= dnsIPAddrs)
          ]
      )

instance Lude.ToPath UpdateConditionalForwarder where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateConditionalForwarder where
  toQuery = Lude.const Lude.mempty

-- | The result of an UpdateConditionalForwarder request.
--
-- /See:/ 'mkUpdateConditionalForwarderResponse' smart constructor.
newtype UpdateConditionalForwarderResponse = UpdateConditionalForwarderResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateConditionalForwarderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateConditionalForwarderResponse
mkUpdateConditionalForwarderResponse pResponseStatus_ =
  UpdateConditionalForwarderResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfrsResponseStatus :: Lens.Lens' UpdateConditionalForwarderResponse Lude.Int
ucfrsResponseStatus = Lens.lens (responseStatus :: UpdateConditionalForwarderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateConditionalForwarderResponse)
{-# DEPRECATED ucfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
