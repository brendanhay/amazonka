{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your AWS Managed Microsoft AD directory, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.
--
-- This action initiates the creation of the AWS side of a trust relationship between an AWS Managed Microsoft AD directory and an external domain. You can create either a forest trust or an external trust.
module Network.AWS.DirectoryService.CreateTrust
  ( -- * Creating a request
    CreateTrust (..),
    mkCreateTrust,

    -- ** Request lenses
    ctDirectoryId,
    ctConditionalForwarderIPAddrs,
    ctTrustDirection,
    ctTrustType,
    ctSelectiveAuth,
    ctRemoteDomainName,
    ctTrustPassword,

    -- * Destructuring the response
    CreateTrustResponse (..),
    mkCreateTrustResponse,

    -- ** Response lenses
    ctrsTrustId,
    ctrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your AWS Managed Microsoft AD directory, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.
--
-- This action initiates the creation of the AWS side of a trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkCreateTrust' smart constructor.
data CreateTrust = CreateTrust'
  { -- | The Directory ID of the AWS Managed Microsoft AD directory for which to establish the trust relationship.
    directoryId :: Lude.Text,
    -- | The IP addresses of the remote DNS server associated with RemoteDomainName.
    conditionalForwarderIPAddrs :: Lude.Maybe [Lude.Text],
    -- | The direction of the trust relationship.
    trustDirection :: TrustDirection,
    -- | The trust relationship type. @Forest@ is the default.
    trustType :: Lude.Maybe TrustType,
    -- | Optional parameter to enable selective authentication for the trust.
    selectiveAuth :: Lude.Maybe SelectiveAuth,
    -- | The Fully Qualified Domain Name (FQDN) of the external domain for which to create the trust relationship.
    remoteDomainName :: Lude.Text,
    -- | The trust password. The must be the same password that was used when creating the trust relationship on the external domain.
    trustPassword :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrust' with the minimum fields required to make a request.
--
-- * 'directoryId' - The Directory ID of the AWS Managed Microsoft AD directory for which to establish the trust relationship.
-- * 'conditionalForwarderIPAddrs' - The IP addresses of the remote DNS server associated with RemoteDomainName.
-- * 'trustDirection' - The direction of the trust relationship.
-- * 'trustType' - The trust relationship type. @Forest@ is the default.
-- * 'selectiveAuth' - Optional parameter to enable selective authentication for the trust.
-- * 'remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain for which to create the trust relationship.
-- * 'trustPassword' - The trust password. The must be the same password that was used when creating the trust relationship on the external domain.
mkCreateTrust ::
  -- | 'directoryId'
  Lude.Text ->
  -- | 'trustDirection'
  TrustDirection ->
  -- | 'remoteDomainName'
  Lude.Text ->
  -- | 'trustPassword'
  Lude.Sensitive Lude.Text ->
  CreateTrust
mkCreateTrust
  pDirectoryId_
  pTrustDirection_
  pRemoteDomainName_
  pTrustPassword_ =
    CreateTrust'
      { directoryId = pDirectoryId_,
        conditionalForwarderIPAddrs = Lude.Nothing,
        trustDirection = pTrustDirection_,
        trustType = Lude.Nothing,
        selectiveAuth = Lude.Nothing,
        remoteDomainName = pRemoteDomainName_,
        trustPassword = pTrustPassword_
      }

-- | The Directory ID of the AWS Managed Microsoft AD directory for which to establish the trust relationship.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDirectoryId :: Lens.Lens' CreateTrust Lude.Text
ctDirectoryId = Lens.lens (directoryId :: CreateTrust -> Lude.Text) (\s a -> s {directoryId = a} :: CreateTrust)
{-# DEPRECATED ctDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The IP addresses of the remote DNS server associated with RemoteDomainName.
--
-- /Note:/ Consider using 'conditionalForwarderIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConditionalForwarderIPAddrs :: Lens.Lens' CreateTrust (Lude.Maybe [Lude.Text])
ctConditionalForwarderIPAddrs = Lens.lens (conditionalForwarderIPAddrs :: CreateTrust -> Lude.Maybe [Lude.Text]) (\s a -> s {conditionalForwarderIPAddrs = a} :: CreateTrust)
{-# DEPRECATED ctConditionalForwarderIPAddrs "Use generic-lens or generic-optics with 'conditionalForwarderIPAddrs' instead." #-}

-- | The direction of the trust relationship.
--
-- /Note:/ Consider using 'trustDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustDirection :: Lens.Lens' CreateTrust TrustDirection
ctTrustDirection = Lens.lens (trustDirection :: CreateTrust -> TrustDirection) (\s a -> s {trustDirection = a} :: CreateTrust)
{-# DEPRECATED ctTrustDirection "Use generic-lens or generic-optics with 'trustDirection' instead." #-}

-- | The trust relationship type. @Forest@ is the default.
--
-- /Note:/ Consider using 'trustType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustType :: Lens.Lens' CreateTrust (Lude.Maybe TrustType)
ctTrustType = Lens.lens (trustType :: CreateTrust -> Lude.Maybe TrustType) (\s a -> s {trustType = a} :: CreateTrust)
{-# DEPRECATED ctTrustType "Use generic-lens or generic-optics with 'trustType' instead." #-}

-- | Optional parameter to enable selective authentication for the trust.
--
-- /Note:/ Consider using 'selectiveAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSelectiveAuth :: Lens.Lens' CreateTrust (Lude.Maybe SelectiveAuth)
ctSelectiveAuth = Lens.lens (selectiveAuth :: CreateTrust -> Lude.Maybe SelectiveAuth) (\s a -> s {selectiveAuth = a} :: CreateTrust)
{-# DEPRECATED ctSelectiveAuth "Use generic-lens or generic-optics with 'selectiveAuth' instead." #-}

-- | The Fully Qualified Domain Name (FQDN) of the external domain for which to create the trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctRemoteDomainName :: Lens.Lens' CreateTrust Lude.Text
ctRemoteDomainName = Lens.lens (remoteDomainName :: CreateTrust -> Lude.Text) (\s a -> s {remoteDomainName = a} :: CreateTrust)
{-# DEPRECATED ctRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The trust password. The must be the same password that was used when creating the trust relationship on the external domain.
--
-- /Note:/ Consider using 'trustPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustPassword :: Lens.Lens' CreateTrust (Lude.Sensitive Lude.Text)
ctTrustPassword = Lens.lens (trustPassword :: CreateTrust -> Lude.Sensitive Lude.Text) (\s a -> s {trustPassword = a} :: CreateTrust)
{-# DEPRECATED ctTrustPassword "Use generic-lens or generic-optics with 'trustPassword' instead." #-}

instance Lude.AWSRequest CreateTrust where
  type Rs CreateTrust = CreateTrustResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTrustResponse'
            Lude.<$> (x Lude..?> "TrustId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTrust where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.CreateTrust" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTrust where
  toJSON CreateTrust' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryId" Lude..= directoryId),
            ("ConditionalForwarderIpAddrs" Lude..=)
              Lude.<$> conditionalForwarderIPAddrs,
            Lude.Just ("TrustDirection" Lude..= trustDirection),
            ("TrustType" Lude..=) Lude.<$> trustType,
            ("SelectiveAuth" Lude..=) Lude.<$> selectiveAuth,
            Lude.Just ("RemoteDomainName" Lude..= remoteDomainName),
            Lude.Just ("TrustPassword" Lude..= trustPassword)
          ]
      )

instance Lude.ToPath CreateTrust where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTrust where
  toQuery = Lude.const Lude.mempty

-- | The result of a CreateTrust request.
--
-- /See:/ 'mkCreateTrustResponse' smart constructor.
data CreateTrustResponse = CreateTrustResponse'
  { -- | A unique identifier for the trust relationship that was created.
    trustId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTrustResponse' with the minimum fields required to make a request.
--
-- * 'trustId' - A unique identifier for the trust relationship that was created.
-- * 'responseStatus' - The response status code.
mkCreateTrustResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTrustResponse
mkCreateTrustResponse pResponseStatus_ =
  CreateTrustResponse'
    { trustId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for the trust relationship that was created.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsTrustId :: Lens.Lens' CreateTrustResponse (Lude.Maybe Lude.Text)
ctrsTrustId = Lens.lens (trustId :: CreateTrustResponse -> Lude.Maybe Lude.Text) (\s a -> s {trustId = a} :: CreateTrustResponse)
{-# DEPRECATED ctrsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTrustResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTrustResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTrustResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
