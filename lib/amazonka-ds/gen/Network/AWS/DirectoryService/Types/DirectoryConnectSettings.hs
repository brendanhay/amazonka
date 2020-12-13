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
    dcsCustomerUserName,
    dcsSubnetIds,
    dcsVPCId,
    dcsCustomerDNSIPs,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information for the 'ConnectDirectory' operation when an AD Connector directory is being created.
--
-- /See:/ 'mkDirectoryConnectSettings' smart constructor.
data DirectoryConnectSettings = DirectoryConnectSettings'
  { -- | The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:
    --
    --
    --     * Read users and groups
    --
    --
    --     * Create computer objects
    --
    --
    --     * Join computers to the domain
    customerUserName :: Lude.Text,
    -- | A list of subnet identifiers in the VPC in which the AD Connector is created.
    subnetIds :: [Lude.Text],
    -- | The identifier of the VPC in which the AD Connector is created.
    vpcId :: Lude.Text,
    -- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
    customerDNSIPs :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryConnectSettings' with the minimum fields required to make a request.
--
-- * 'customerUserName' - The user name of an account in the on-premises directory that is used to connect to the directory. This account must have the following permissions:
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
-- * 'subnetIds' - A list of subnet identifiers in the VPC in which the AD Connector is created.
-- * 'vpcId' - The identifier of the VPC in which the AD Connector is created.
-- * 'customerDNSIPs' - A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
mkDirectoryConnectSettings ::
  -- | 'customerUserName'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  DirectoryConnectSettings
mkDirectoryConnectSettings pCustomerUserName_ pVPCId_ =
  DirectoryConnectSettings'
    { customerUserName = pCustomerUserName_,
      subnetIds = Lude.mempty,
      vpcId = pVPCId_,
      customerDNSIPs = Lude.mempty
    }

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
dcsCustomerUserName :: Lens.Lens' DirectoryConnectSettings Lude.Text
dcsCustomerUserName = Lens.lens (customerUserName :: DirectoryConnectSettings -> Lude.Text) (\s a -> s {customerUserName = a} :: DirectoryConnectSettings)
{-# DEPRECATED dcsCustomerUserName "Use generic-lens or generic-optics with 'customerUserName' instead." #-}

-- | A list of subnet identifiers in the VPC in which the AD Connector is created.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSubnetIds :: Lens.Lens' DirectoryConnectSettings [Lude.Text]
dcsSubnetIds = Lens.lens (subnetIds :: DirectoryConnectSettings -> [Lude.Text]) (\s a -> s {subnetIds = a} :: DirectoryConnectSettings)
{-# DEPRECATED dcsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The identifier of the VPC in which the AD Connector is created.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsVPCId :: Lens.Lens' DirectoryConnectSettings Lude.Text
dcsVPCId = Lens.lens (vpcId :: DirectoryConnectSettings -> Lude.Text) (\s a -> s {vpcId = a} :: DirectoryConnectSettings)
{-# DEPRECATED dcsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of one or more IP addresses of DNS servers or domain controllers in the on-premises directory.
--
-- /Note:/ Consider using 'customerDNSIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsCustomerDNSIPs :: Lens.Lens' DirectoryConnectSettings [Lude.Text]
dcsCustomerDNSIPs = Lens.lens (customerDNSIPs :: DirectoryConnectSettings -> [Lude.Text]) (\s a -> s {customerDNSIPs = a} :: DirectoryConnectSettings)
{-# DEPRECATED dcsCustomerDNSIPs "Use generic-lens or generic-optics with 'customerDNSIPs' instead." #-}

instance Lude.ToJSON DirectoryConnectSettings where
  toJSON DirectoryConnectSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CustomerUserName" Lude..= customerUserName),
            Lude.Just ("SubnetIds" Lude..= subnetIds),
            Lude.Just ("VpcId" Lude..= vpcId),
            Lude.Just ("CustomerDnsIps" Lude..= customerDNSIPs)
          ]
      )
