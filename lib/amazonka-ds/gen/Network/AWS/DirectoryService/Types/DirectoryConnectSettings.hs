{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettings
  ( DirectoryConnectSettings (..),

    -- * Smart constructor
    mkDirectoryConnectSettings,

    -- * Lenses
    dcsVpcId,
    dcsSubnetIds,
    dcsCustomerDnsIps,
    dcsCustomerUserName,
  )
where

import qualified Network.AWS.DirectoryService.Types.IpAddr as Types
import qualified Network.AWS.DirectoryService.Types.SubnetId as Types
import qualified Network.AWS.DirectoryService.Types.UserName as Types
import qualified Network.AWS.DirectoryService.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information for the 'ConnectDirectory' operation when an AD Connector directory is being created.
--
-- /See:/ 'mkDirectoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { -- | The identifier of the VPC in which the AD Connector is created.
    vpcId :: Types.VpcId,
    -- | A list of subnet identifiers in the VPC in which the AD Connector is created.
    subnetIds :: [Types.SubnetId],
    -- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
    customerDnsIps :: [Types.IpAddr],
    -- | The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:
    --
    --
    --     * Read users and groups
    --
    --
    --     * Create computer objects
    --
    --
    --     * Join computers to the domain
    customerUserName :: Types.UserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryConnectSettings' value with any optional fields omitted.
mkDirectoryConnectSettings ::
  -- | 'vpcId'
  Types.VpcId ->
  -- | 'customerUserName'
  Types.UserName ->
  DirectoryConnectSettings
mkDirectoryConnectSettings vpcId customerUserName =
  DirectoryConnectSettings'
    { vpcId,
      subnetIds = Core.mempty,
      customerDnsIps = Core.mempty,
      customerUserName
    }

-- | The identifier of the VPC in which the AD Connector is created.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsVpcId :: Lens.Lens' DirectoryConnectSettings Types.VpcId
dcsVpcId = Lens.field @"vpcId"
{-# DEPRECATED dcsVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of subnet identifiers in the VPC in which the AD Connector is created.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSubnetIds :: Lens.Lens' DirectoryConnectSettings [Types.SubnetId]
dcsSubnetIds = Lens.field @"subnetIds"
{-# DEPRECATED dcsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
--
-- /Note:/ Consider using 'customerDnsIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCustomerDnsIps :: Lens.Lens' DirectoryConnectSettings [Types.IpAddr]
dcsCustomerDnsIps = Lens.field @"customerDnsIps"
{-# DEPRECATED dcsCustomerDnsIps "Use generic-lens or generic-optics with 'customerDnsIps' instead." #-}

-- | The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:
--
--
--     * Read users and groups
--
--
--     * Create computer objects
--
--
--     * Join computers to the domain
--
--
--
-- /Note:/ Consider using 'customerUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCustomerUserName :: Lens.Lens' DirectoryConnectSettings Types.UserName
dcsCustomerUserName = Lens.field @"customerUserName"
{-# DEPRECATED dcsCustomerUserName "Use generic-lens or generic-optics with 'customerUserName' instead." #-}

instance Core.FromJSON DirectoryConnectSettings where
  toJSON DirectoryConnectSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VpcId" Core..= vpcId),
            Core.Just ("SubnetIds" Core..= subnetIds),
            Core.Just ("CustomerDnsIps" Core..= customerDnsIps),
            Core.Just ("CustomerUserName" Core..= customerUserName)
          ]
      )
