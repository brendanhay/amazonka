{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
  ( OwnerDirectoryDescription (..)
  -- * Smart constructor
  , mkOwnerDirectoryDescription
  -- * Lenses
  , oddAccountId
  , oddDirectoryId
  , oddDnsIpAddrs
  , oddRadiusSettings
  , oddRadiusStatus
  , oddVpcSettings
  ) where

import qualified Network.AWS.DirectoryService.Types.CustomerId as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription as Types
import qualified Network.AWS.DirectoryService.Types.IpAddr as Types
import qualified Network.AWS.DirectoryService.Types.RadiusSettings as Types
import qualified Network.AWS.DirectoryService.Types.RadiusStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the directory owner account details that have been shared to the directory consumer account.
--
-- /See:/ 'mkOwnerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { accountId :: Core.Maybe Types.CustomerId
    -- ^ Identifier of the directory owner account.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
  , dnsIpAddrs :: Core.Maybe [Types.IpAddr]
    -- ^ IP address of the directory’s domain controllers.
  , radiusSettings :: Core.Maybe Types.RadiusSettings
    -- ^ A 'RadiusSettings' object that contains information about the RADIUS server.
  , radiusStatus :: Core.Maybe Types.RadiusStatus
    -- ^ Information about the status of the RADIUS server.
  , vpcSettings :: Core.Maybe Types.DirectoryVpcSettingsDescription
    -- ^ Information about the VPC settings for the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OwnerDirectoryDescription' value with any optional fields omitted.
mkOwnerDirectoryDescription
    :: OwnerDirectoryDescription
mkOwnerDirectoryDescription
  = OwnerDirectoryDescription'{accountId = Core.Nothing,
                               directoryId = Core.Nothing, dnsIpAddrs = Core.Nothing,
                               radiusSettings = Core.Nothing, radiusStatus = Core.Nothing,
                               vpcSettings = Core.Nothing}

-- | Identifier of the directory owner account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddAccountId :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Types.CustomerId)
oddAccountId = Lens.field @"accountId"
{-# INLINEABLE oddAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddDirectoryId :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Types.DirectoryId)
oddDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE oddDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | IP address of the directory’s domain controllers.
--
-- /Note:/ Consider using 'dnsIpAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddDnsIpAddrs :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe [Types.IpAddr])
oddDnsIpAddrs = Lens.field @"dnsIpAddrs"
{-# INLINEABLE oddDnsIpAddrs #-}
{-# DEPRECATED dnsIpAddrs "Use generic-lens or generic-optics with 'dnsIpAddrs' instead"  #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddRadiusSettings :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Types.RadiusSettings)
oddRadiusSettings = Lens.field @"radiusSettings"
{-# INLINEABLE oddRadiusSettings #-}
{-# DEPRECATED radiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead"  #-}

-- | Information about the status of the RADIUS server.
--
-- /Note:/ Consider using 'radiusStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddRadiusStatus :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Types.RadiusStatus)
oddRadiusStatus = Lens.field @"radiusStatus"
{-# INLINEABLE oddRadiusStatus #-}
{-# DEPRECATED radiusStatus "Use generic-lens or generic-optics with 'radiusStatus' instead"  #-}

-- | Information about the VPC settings for the directory.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddVpcSettings :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Types.DirectoryVpcSettingsDescription)
oddVpcSettings = Lens.field @"vpcSettings"
{-# INLINEABLE oddVpcSettings #-}
{-# DEPRECATED vpcSettings "Use generic-lens or generic-optics with 'vpcSettings' instead"  #-}

instance Core.FromJSON OwnerDirectoryDescription where
        parseJSON
          = Core.withObject "OwnerDirectoryDescription" Core.$
              \ x ->
                OwnerDirectoryDescription' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "DirectoryId" Core.<*>
                    x Core..:? "DnsIpAddrs"
                    Core.<*> x Core..:? "RadiusSettings"
                    Core.<*> x Core..:? "RadiusStatus"
                    Core.<*> x Core..:? "VpcSettings"
