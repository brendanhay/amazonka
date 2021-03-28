{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Interconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.Interconnect
  ( Interconnect (..)
  -- * Smart constructor
  , mkInterconnect
  -- * Lenses
  , iAwsDevice
  , iAwsDeviceV2
  , iBandwidth
  , iHasLogicalRedundancy
  , iInterconnectId
  , iInterconnectName
  , iInterconnectState
  , iJumboFrameCapable
  , iLagId
  , iLoaIssueTime
  , iLocation
  , iProviderName
  , iRegion
  , iTags
  ) where

import qualified Network.AWS.DirectConnect.Types.AwsDevice as Types
import qualified Network.AWS.DirectConnect.Types.AwsDeviceV2 as Types
import qualified Network.AWS.DirectConnect.Types.Bandwidth as Types
import qualified Network.AWS.DirectConnect.Types.HasLogicalRedundancy as Types
import qualified Network.AWS.DirectConnect.Types.InterconnectId as Types
import qualified Network.AWS.DirectConnect.Types.InterconnectName as Types
import qualified Network.AWS.DirectConnect.Types.InterconnectState as Types
import qualified Network.AWS.DirectConnect.Types.LagId as Types
import qualified Network.AWS.DirectConnect.Types.LocationCode as Types
import qualified Network.AWS.DirectConnect.Types.ProviderName as Types
import qualified Network.AWS.DirectConnect.Types.Region as Types
import qualified Network.AWS.DirectConnect.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an interconnect.
--
-- /See:/ 'mkInterconnect' smart constructor.
data Interconnect = Interconnect'
  { awsDevice :: Core.Maybe Types.AwsDevice
    -- ^ The Direct Connect endpoint on which the physical connection terminates.
  , awsDeviceV2 :: Core.Maybe Types.AwsDeviceV2
    -- ^ The Direct Connect endpoint on which the physical connection terminates.
  , bandwidth :: Core.Maybe Types.Bandwidth
    -- ^ The bandwidth of the connection.
  , hasLogicalRedundancy :: Core.Maybe Types.HasLogicalRedundancy
    -- ^ Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
  , interconnectId :: Core.Maybe Types.InterconnectId
    -- ^ The ID of the interconnect.
  , interconnectName :: Core.Maybe Types.InterconnectName
    -- ^ The name of the interconnect.
  , interconnectState :: Core.Maybe Types.InterconnectState
    -- ^ The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
  , jumboFrameCapable :: Core.Maybe Core.Bool
    -- ^ Indicates whether jumbo frames (9001 MTU) are supported.
  , lagId :: Core.Maybe Types.LagId
    -- ^ The ID of the LAG.
  , loaIssueTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time of the most recent call to 'DescribeLoa' for this connection.
  , location :: Core.Maybe Types.LocationCode
    -- ^ The location of the connection.
  , providerName :: Core.Maybe Types.ProviderName
    -- ^ The name of the service provider associated with the interconnect.
  , region :: Core.Maybe Types.Region
    -- ^ The AWS Region where the connection is located.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags associated with the interconnect.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Interconnect' value with any optional fields omitted.
mkInterconnect
    :: Interconnect
mkInterconnect
  = Interconnect'{awsDevice = Core.Nothing,
                  awsDeviceV2 = Core.Nothing, bandwidth = Core.Nothing,
                  hasLogicalRedundancy = Core.Nothing, interconnectId = Core.Nothing,
                  interconnectName = Core.Nothing, interconnectState = Core.Nothing,
                  jumboFrameCapable = Core.Nothing, lagId = Core.Nothing,
                  loaIssueTime = Core.Nothing, location = Core.Nothing,
                  providerName = Core.Nothing, region = Core.Nothing,
                  tags = Core.Nothing}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAwsDevice :: Lens.Lens' Interconnect (Core.Maybe Types.AwsDevice)
iAwsDevice = Lens.field @"awsDevice"
{-# INLINEABLE iAwsDevice #-}
{-# DEPRECATED awsDevice "Use generic-lens or generic-optics with 'awsDevice' instead"  #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAwsDeviceV2 :: Lens.Lens' Interconnect (Core.Maybe Types.AwsDeviceV2)
iAwsDeviceV2 = Lens.field @"awsDeviceV2"
{-# INLINEABLE iAwsDeviceV2 #-}
{-# DEPRECATED awsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead"  #-}

-- | The bandwidth of the connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBandwidth :: Lens.Lens' Interconnect (Core.Maybe Types.Bandwidth)
iBandwidth = Lens.field @"bandwidth"
{-# INLINEABLE iBandwidth #-}
{-# DEPRECATED bandwidth "Use generic-lens or generic-optics with 'bandwidth' instead"  #-}

-- | Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHasLogicalRedundancy :: Lens.Lens' Interconnect (Core.Maybe Types.HasLogicalRedundancy)
iHasLogicalRedundancy = Lens.field @"hasLogicalRedundancy"
{-# INLINEABLE iHasLogicalRedundancy #-}
{-# DEPRECATED hasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead"  #-}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInterconnectId :: Lens.Lens' Interconnect (Core.Maybe Types.InterconnectId)
iInterconnectId = Lens.field @"interconnectId"
{-# INLINEABLE iInterconnectId #-}
{-# DEPRECATED interconnectId "Use generic-lens or generic-optics with 'interconnectId' instead"  #-}

-- | The name of the interconnect.
--
-- /Note:/ Consider using 'interconnectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInterconnectName :: Lens.Lens' Interconnect (Core.Maybe Types.InterconnectName)
iInterconnectName = Lens.field @"interconnectName"
{-# INLINEABLE iInterconnectName #-}
{-# DEPRECATED interconnectName "Use generic-lens or generic-optics with 'interconnectName' instead"  #-}

-- | The state of the interconnect. The following are the possible values:
--
--
--     * @requested@ : The initial state of an interconnect. The interconnect stays in the requested state until the Letter of Authorization (LOA) is sent to the customer.
--
--
--     * @pending@ : The interconnect is approved, and is being initialized.
--
--
--     * @available@ : The network link is up, and the interconnect is ready for use.
--
--
--     * @down@ : The network link is down.
--
--
--     * @deleting@ : The interconnect is being deleted.
--
--
--     * @deleted@ : The interconnect is deleted.
--
--
--     * @unknown@ : The state of the interconnect is not available.
--
--
--
-- /Note:/ Consider using 'interconnectState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInterconnectState :: Lens.Lens' Interconnect (Core.Maybe Types.InterconnectState)
iInterconnectState = Lens.field @"interconnectState"
{-# INLINEABLE iInterconnectState #-}
{-# DEPRECATED interconnectState "Use generic-lens or generic-optics with 'interconnectState' instead"  #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iJumboFrameCapable :: Lens.Lens' Interconnect (Core.Maybe Core.Bool)
iJumboFrameCapable = Lens.field @"jumboFrameCapable"
{-# INLINEABLE iJumboFrameCapable #-}
{-# DEPRECATED jumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead"  #-}

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLagId :: Lens.Lens' Interconnect (Core.Maybe Types.LagId)
iLagId = Lens.field @"lagId"
{-# INLINEABLE iLagId #-}
{-# DEPRECATED lagId "Use generic-lens or generic-optics with 'lagId' instead"  #-}

-- | The time of the most recent call to 'DescribeLoa' for this connection.
--
-- /Note:/ Consider using 'loaIssueTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLoaIssueTime :: Lens.Lens' Interconnect (Core.Maybe Core.NominalDiffTime)
iLoaIssueTime = Lens.field @"loaIssueTime"
{-# INLINEABLE iLoaIssueTime #-}
{-# DEPRECATED loaIssueTime "Use generic-lens or generic-optics with 'loaIssueTime' instead"  #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLocation :: Lens.Lens' Interconnect (Core.Maybe Types.LocationCode)
iLocation = Lens.field @"location"
{-# INLINEABLE iLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the service provider associated with the interconnect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProviderName :: Lens.Lens' Interconnect (Core.Maybe Types.ProviderName)
iProviderName = Lens.field @"providerName"
{-# INLINEABLE iProviderName #-}
{-# DEPRECATED providerName "Use generic-lens or generic-optics with 'providerName' instead"  #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegion :: Lens.Lens' Interconnect (Core.Maybe Types.Region)
iRegion = Lens.field @"region"
{-# INLINEABLE iRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The tags associated with the interconnect.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Interconnect (Core.Maybe (Core.NonEmpty Types.Tag))
iTags = Lens.field @"tags"
{-# INLINEABLE iTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON Interconnect where
        parseJSON
          = Core.withObject "Interconnect" Core.$
              \ x ->
                Interconnect' Core.<$>
                  (x Core..:? "awsDevice") Core.<*> x Core..:? "awsDeviceV2" Core.<*>
                    x Core..:? "bandwidth"
                    Core.<*> x Core..:? "hasLogicalRedundancy"
                    Core.<*> x Core..:? "interconnectId"
                    Core.<*> x Core..:? "interconnectName"
                    Core.<*> x Core..:? "interconnectState"
                    Core.<*> x Core..:? "jumboFrameCapable"
                    Core.<*> x Core..:? "lagId"
                    Core.<*> x Core..:? "loaIssueTime"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "providerName"
                    Core.<*> x Core..:? "region"
                    Core.<*> x Core..:? "tags"
