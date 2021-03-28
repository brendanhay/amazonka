{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IpRouteInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.IpRouteInfo
  ( IpRouteInfo (..)
  -- * Smart constructor
  , mkIpRouteInfo
  -- * Lenses
  , iriAddedDateTime
  , iriCidrIp
  , iriDescription
  , iriDirectoryId
  , iriIpRouteStatusMsg
  , iriIpRouteStatusReason
  ) where

import qualified Network.AWS.DirectoryService.Types.CidrIp as Types
import qualified Network.AWS.DirectoryService.Types.Description as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.IpRouteStatusMsg as Types
import qualified Network.AWS.DirectoryService.Types.IpRouteStatusReason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about one or more IP address blocks.
--
-- /See:/ 'mkIpRouteInfo' smart constructor.
data IpRouteInfo = IpRouteInfo'
  { addedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the address block was added to the directory.
  , cidrIp :: Core.Maybe Types.CidrIp
    -- ^ IP address block in the 'IpRoute' .
  , description :: Core.Maybe Types.Description
    -- ^ Description of the 'IpRouteInfo' .
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ Identifier (ID) of the directory associated with the IP addresses.
  , ipRouteStatusMsg :: Core.Maybe Types.IpRouteStatusMsg
    -- ^ The status of the IP address block.
  , ipRouteStatusReason :: Core.Maybe Types.IpRouteStatusReason
    -- ^ The reason for the IpRouteStatusMsg.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'IpRouteInfo' value with any optional fields omitted.
mkIpRouteInfo
    :: IpRouteInfo
mkIpRouteInfo
  = IpRouteInfo'{addedDateTime = Core.Nothing, cidrIp = Core.Nothing,
                 description = Core.Nothing, directoryId = Core.Nothing,
                 ipRouteStatusMsg = Core.Nothing,
                 ipRouteStatusReason = Core.Nothing}

-- | The date and time the address block was added to the directory.
--
-- /Note:/ Consider using 'addedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriAddedDateTime :: Lens.Lens' IpRouteInfo (Core.Maybe Core.NominalDiffTime)
iriAddedDateTime = Lens.field @"addedDateTime"
{-# INLINEABLE iriAddedDateTime #-}
{-# DEPRECATED addedDateTime "Use generic-lens or generic-optics with 'addedDateTime' instead"  #-}

-- | IP address block in the 'IpRoute' .
--
-- /Note:/ Consider using 'cidrIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriCidrIp :: Lens.Lens' IpRouteInfo (Core.Maybe Types.CidrIp)
iriCidrIp = Lens.field @"cidrIp"
{-# INLINEABLE iriCidrIp #-}
{-# DEPRECATED cidrIp "Use generic-lens or generic-optics with 'cidrIp' instead"  #-}

-- | Description of the 'IpRouteInfo' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriDescription :: Lens.Lens' IpRouteInfo (Core.Maybe Types.Description)
iriDescription = Lens.field @"description"
{-# INLINEABLE iriDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Identifier (ID) of the directory associated with the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriDirectoryId :: Lens.Lens' IpRouteInfo (Core.Maybe Types.DirectoryId)
iriDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE iriDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The status of the IP address block.
--
-- /Note:/ Consider using 'ipRouteStatusMsg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIpRouteStatusMsg :: Lens.Lens' IpRouteInfo (Core.Maybe Types.IpRouteStatusMsg)
iriIpRouteStatusMsg = Lens.field @"ipRouteStatusMsg"
{-# INLINEABLE iriIpRouteStatusMsg #-}
{-# DEPRECATED ipRouteStatusMsg "Use generic-lens or generic-optics with 'ipRouteStatusMsg' instead"  #-}

-- | The reason for the IpRouteStatusMsg.
--
-- /Note:/ Consider using 'ipRouteStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIpRouteStatusReason :: Lens.Lens' IpRouteInfo (Core.Maybe Types.IpRouteStatusReason)
iriIpRouteStatusReason = Lens.field @"ipRouteStatusReason"
{-# INLINEABLE iriIpRouteStatusReason #-}
{-# DEPRECATED ipRouteStatusReason "Use generic-lens or generic-optics with 'ipRouteStatusReason' instead"  #-}

instance Core.FromJSON IpRouteInfo where
        parseJSON
          = Core.withObject "IpRouteInfo" Core.$
              \ x ->
                IpRouteInfo' Core.<$>
                  (x Core..:? "AddedDateTime") Core.<*> x Core..:? "CidrIp" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "IpRouteStatusMsg"
                    Core.<*> x Core..:? "IpRouteStatusReason"
