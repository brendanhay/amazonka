{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTrust (..)
    , mkCreateTrust
    -- ** Request lenses
    , ctDirectoryId
    , ctRemoteDomainName
    , ctTrustPassword
    , ctTrustDirection
    , ctConditionalForwarderIpAddrs
    , ctSelectiveAuth
    , ctTrustType

    -- * Destructuring the response
    , CreateTrustResponse (..)
    , mkCreateTrustResponse
    -- ** Response lenses
    , ctrrsTrustId
    , ctrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your AWS Managed Microsoft AD directory, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.
--
-- This action initiates the creation of the AWS side of a trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkCreateTrust' smart constructor.
data CreateTrust = CreateTrust'
  { directoryId :: Types.DirectoryId
    -- ^ The Directory ID of the AWS Managed Microsoft AD directory for which to establish the trust relationship.
  , remoteDomainName :: Types.RemoteDomainName
    -- ^ The Fully Qualified Domain Name (FQDN) of the external domain for which to create the trust relationship.
  , trustPassword :: Types.TrustPassword
    -- ^ The trust password. The must be the same password that was used when creating the trust relationship on the external domain.
  , trustDirection :: Types.TrustDirection
    -- ^ The direction of the trust relationship.
  , conditionalForwarderIpAddrs :: Core.Maybe [Types.IpAddr]
    -- ^ The IP addresses of the remote DNS server associated with RemoteDomainName.
  , selectiveAuth :: Core.Maybe Types.SelectiveAuth
    -- ^ Optional parameter to enable selective authentication for the trust.
  , trustType :: Core.Maybe Types.TrustType
    -- ^ The trust relationship type. @Forest@ is the default.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrust' value with any optional fields omitted.
mkCreateTrust
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.RemoteDomainName -- ^ 'remoteDomainName'
    -> Types.TrustPassword -- ^ 'trustPassword'
    -> Types.TrustDirection -- ^ 'trustDirection'
    -> CreateTrust
mkCreateTrust directoryId remoteDomainName trustPassword
  trustDirection
  = CreateTrust'{directoryId, remoteDomainName, trustPassword,
                 trustDirection, conditionalForwarderIpAddrs = Core.Nothing,
                 selectiveAuth = Core.Nothing, trustType = Core.Nothing}

-- | The Directory ID of the AWS Managed Microsoft AD directory for which to establish the trust relationship.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctDirectoryId :: Lens.Lens' CreateTrust Types.DirectoryId
ctDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE ctDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The Fully Qualified Domain Name (FQDN) of the external domain for which to create the trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctRemoteDomainName :: Lens.Lens' CreateTrust Types.RemoteDomainName
ctRemoteDomainName = Lens.field @"remoteDomainName"
{-# INLINEABLE ctRemoteDomainName #-}
{-# DEPRECATED remoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead"  #-}

-- | The trust password. The must be the same password that was used when creating the trust relationship on the external domain.
--
-- /Note:/ Consider using 'trustPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustPassword :: Lens.Lens' CreateTrust Types.TrustPassword
ctTrustPassword = Lens.field @"trustPassword"
{-# INLINEABLE ctTrustPassword #-}
{-# DEPRECATED trustPassword "Use generic-lens or generic-optics with 'trustPassword' instead"  #-}

-- | The direction of the trust relationship.
--
-- /Note:/ Consider using 'trustDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustDirection :: Lens.Lens' CreateTrust Types.TrustDirection
ctTrustDirection = Lens.field @"trustDirection"
{-# INLINEABLE ctTrustDirection #-}
{-# DEPRECATED trustDirection "Use generic-lens or generic-optics with 'trustDirection' instead"  #-}

-- | The IP addresses of the remote DNS server associated with RemoteDomainName.
--
-- /Note:/ Consider using 'conditionalForwarderIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctConditionalForwarderIpAddrs :: Lens.Lens' CreateTrust (Core.Maybe [Types.IpAddr])
ctConditionalForwarderIpAddrs = Lens.field @"conditionalForwarderIpAddrs"
{-# INLINEABLE ctConditionalForwarderIpAddrs #-}
{-# DEPRECATED conditionalForwarderIpAddrs "Use generic-lens or generic-optics with 'conditionalForwarderIpAddrs' instead"  #-}

-- | Optional parameter to enable selective authentication for the trust.
--
-- /Note:/ Consider using 'selectiveAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctSelectiveAuth :: Lens.Lens' CreateTrust (Core.Maybe Types.SelectiveAuth)
ctSelectiveAuth = Lens.field @"selectiveAuth"
{-# INLINEABLE ctSelectiveAuth #-}
{-# DEPRECATED selectiveAuth "Use generic-lens or generic-optics with 'selectiveAuth' instead"  #-}

-- | The trust relationship type. @Forest@ is the default.
--
-- /Note:/ Consider using 'trustType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTrustType :: Lens.Lens' CreateTrust (Core.Maybe Types.TrustType)
ctTrustType = Lens.field @"trustType"
{-# INLINEABLE ctTrustType #-}
{-# DEPRECATED trustType "Use generic-lens or generic-optics with 'trustType' instead"  #-}

instance Core.ToQuery CreateTrust where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateTrust where
        toHeaders CreateTrust{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.CreateTrust")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateTrust where
        toJSON CreateTrust{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("RemoteDomainName" Core..= remoteDomainName),
                  Core.Just ("TrustPassword" Core..= trustPassword),
                  Core.Just ("TrustDirection" Core..= trustDirection),
                  ("ConditionalForwarderIpAddrs" Core..=) Core.<$>
                    conditionalForwarderIpAddrs,
                  ("SelectiveAuth" Core..=) Core.<$> selectiveAuth,
                  ("TrustType" Core..=) Core.<$> trustType])

instance Core.AWSRequest CreateTrust where
        type Rs CreateTrust = CreateTrustResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateTrustResponse' Core.<$>
                   (x Core..:? "TrustId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The result of a CreateTrust request.
--
-- /See:/ 'mkCreateTrustResponse' smart constructor.
data CreateTrustResponse = CreateTrustResponse'
  { trustId :: Core.Maybe Types.TrustId
    -- ^ A unique identifier for the trust relationship that was created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTrustResponse' value with any optional fields omitted.
mkCreateTrustResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTrustResponse
mkCreateTrustResponse responseStatus
  = CreateTrustResponse'{trustId = Core.Nothing, responseStatus}

-- | A unique identifier for the trust relationship that was created.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsTrustId :: Lens.Lens' CreateTrustResponse (Core.Maybe Types.TrustId)
ctrrsTrustId = Lens.field @"trustId"
{-# INLINEABLE ctrrsTrustId #-}
{-# DEPRECATED trustId "Use generic-lens or generic-optics with 'trustId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrrsResponseStatus :: Lens.Lens' CreateTrustResponse Core.Int
ctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
