{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateConditionalForwarder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.
module Network.AWS.DirectoryService.CreateConditionalForwarder
  ( -- * Creating a request
    CreateConditionalForwarder (..),
    mkCreateConditionalForwarder,

    -- ** Request lenses
    ccfDirectoryId,
    ccfRemoteDomainName,
    ccfDNSIPAddrs,

    -- * Destructuring the response
    CreateConditionalForwarderResponse (..),
    mkCreateConditionalForwarderResponse,

    -- ** Response lenses
    ccfrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.
--
-- /See:/ 'mkCreateConditionalForwarder' smart constructor.
data CreateConditionalForwarder = CreateConditionalForwarder'
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

-- | Creates a value of 'CreateConditionalForwarder' with the minimum fields required to make a request.
--
-- * 'directoryId' - The directory ID of the AWS directory for which you are creating the conditional forwarder.
-- * 'dnsIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName.
-- * 'remoteDomainName' - The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
mkCreateConditionalForwarder ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'remoteDomainName'
  Lude.Text ->
  CreateConditionalForwarder
mkCreateConditionalForwarder pDirectoryId_ pRemoteDomainName_ =
  CreateConditionalForwarder'
    { directoryId = pDirectoryId_,
      remoteDomainName = pRemoteDomainName_,
      dnsIPAddrs = Lude.mempty
    }

-- | The directory ID of the AWS directory for which you are creating the conditional forwarder.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDirectoryId :: Lens.Lens' CreateConditionalForwarder Lude.Text
ccfDirectoryId = Lens.lens (directoryId :: CreateConditionalForwarder -> Lude.Text) (\s a -> s {directoryId = a} :: CreateConditionalForwarder)
{-# DEPRECATED ccfDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The fully qualified domain name (FQDN) of the remote domain with which you will set up a trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfRemoteDomainName :: Lens.Lens' CreateConditionalForwarder Lude.Text
ccfRemoteDomainName = Lens.lens (remoteDomainName :: CreateConditionalForwarder -> Lude.Text) (\s a -> s {remoteDomainName = a} :: CreateConditionalForwarder)
{-# DEPRECATED ccfRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The IP addresses of the remote DNS server associated with RemoteDomainName.
--
-- /Note:/ Consider using 'dnsIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDNSIPAddrs :: Lens.Lens' CreateConditionalForwarder [Lude.Text]
ccfDNSIPAddrs = Lens.lens (dnsIPAddrs :: CreateConditionalForwarder -> [Lude.Text]) (\s a -> s {dnsIPAddrs = a} :: CreateConditionalForwarder)
{-# DEPRECATED ccfDNSIPAddrs "Use generic-lens or generic-optics with 'dnsIPAddrs' instead." #-}

instance Lude.AWSRequest CreateConditionalForwarder where
  type
    Rs CreateConditionalForwarder =
      CreateConditionalForwarderResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateConditionalForwarderResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateConditionalForwarder where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.CreateConditionalForwarder" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateConditionalForwarder where
  toJSON CreateConditionalForwarder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            Lude.Just ("RemoteDomainName" Lude..= remoteDomainName),
            Lude.Just ("DnsIpAddrs" Lude..= dnsIPAddrs)
          ]
      )

instance Lude.ToPath CreateConditionalForwarder where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateConditionalForwarder where
  toQuery = Lude.const Lude.mempty

-- | The result of a CreateConditinalForwarder request.
--
-- /See:/ 'mkCreateConditionalForwarderResponse' smart constructor.
newtype CreateConditionalForwarderResponse = CreateConditionalForwarderResponse'
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

-- | Creates a value of 'CreateConditionalForwarderResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateConditionalForwarderResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateConditionalForwarderResponse
mkCreateConditionalForwarderResponse pResponseStatus_ =
  CreateConditionalForwarderResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrsResponseStatus :: Lens.Lens' CreateConditionalForwarderResponse Lude.Int
ccfrsResponseStatus = Lens.lens (responseStatus :: CreateConditionalForwarderResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateConditionalForwarderResponse)
{-# DEPRECATED ccfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
