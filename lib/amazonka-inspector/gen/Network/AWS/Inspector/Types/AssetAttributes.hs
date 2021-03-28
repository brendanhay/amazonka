{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssetAttributes
  ( AssetAttributes (..)
  -- * Smart constructor
  , mkAssetAttributes
  -- * Lenses
  , aaSchemaVersion
  , aaAgentId
  , aaAmiId
  , aaAutoScalingGroup
  , aaHostname
  , aaIpv4Addresses
  , aaNetworkInterfaces
  , aaTags
  ) where

import qualified Network.AWS.Inspector.Types.AgentId as Types
import qualified Network.AWS.Inspector.Types.AmiId as Types
import qualified Network.AWS.Inspector.Types.AutoScalingGroup as Types
import qualified Network.AWS.Inspector.Types.Hostname as Types
import qualified Network.AWS.Inspector.Types.Ipv4Address as Types
import qualified Network.AWS.Inspector.Types.NetworkInterface as Types
import qualified Network.AWS.Inspector.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A collection of attributes of the host from which the finding is generated.
--
-- /See:/ 'mkAssetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { schemaVersion :: Core.Natural
    -- ^ The schema version of this data type.
  , agentId :: Core.Maybe Types.AgentId
    -- ^ The ID of the agent that is installed on the EC2 instance where the finding is generated.
  , amiId :: Core.Maybe Types.AmiId
    -- ^ The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
  , autoScalingGroup :: Core.Maybe Types.AutoScalingGroup
    -- ^ The Auto Scaling group of the EC2 instance where the finding is generated.
  , hostname :: Core.Maybe Types.Hostname
    -- ^ The hostname of the EC2 instance where the finding is generated.
  , ipv4Addresses :: Core.Maybe [Types.Ipv4Address]
    -- ^ The list of IP v4 addresses of the EC2 instance where the finding is generated.
  , networkInterfaces :: Core.Maybe [Types.NetworkInterface]
    -- ^ An array of the network interfaces interacting with the EC2 instance where the finding is generated.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags related to the EC2 instance where the finding is generated.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssetAttributes' value with any optional fields omitted.
mkAssetAttributes
    :: Core.Natural -- ^ 'schemaVersion'
    -> AssetAttributes
mkAssetAttributes schemaVersion
  = AssetAttributes'{schemaVersion, agentId = Core.Nothing,
                     amiId = Core.Nothing, autoScalingGroup = Core.Nothing,
                     hostname = Core.Nothing, ipv4Addresses = Core.Nothing,
                     networkInterfaces = Core.Nothing, tags = Core.Nothing}

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaSchemaVersion :: Lens.Lens' AssetAttributes Core.Natural
aaSchemaVersion = Lens.field @"schemaVersion"
{-# INLINEABLE aaSchemaVersion #-}
{-# DEPRECATED schemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead"  #-}

-- | The ID of the agent that is installed on the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAgentId :: Lens.Lens' AssetAttributes (Core.Maybe Types.AgentId)
aaAgentId = Lens.field @"agentId"
{-# INLINEABLE aaAgentId #-}
{-# DEPRECATED agentId "Use generic-lens or generic-optics with 'agentId' instead"  #-}

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAmiId :: Lens.Lens' AssetAttributes (Core.Maybe Types.AmiId)
aaAmiId = Lens.field @"amiId"
{-# INLINEABLE aaAmiId #-}
{-# DEPRECATED amiId "Use generic-lens or generic-optics with 'amiId' instead"  #-}

-- | The Auto Scaling group of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'autoScalingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAutoScalingGroup :: Lens.Lens' AssetAttributes (Core.Maybe Types.AutoScalingGroup)
aaAutoScalingGroup = Lens.field @"autoScalingGroup"
{-# INLINEABLE aaAutoScalingGroup #-}
{-# DEPRECATED autoScalingGroup "Use generic-lens or generic-optics with 'autoScalingGroup' instead"  #-}

-- | The hostname of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaHostname :: Lens.Lens' AssetAttributes (Core.Maybe Types.Hostname)
aaHostname = Lens.field @"hostname"
{-# INLINEABLE aaHostname #-}
{-# DEPRECATED hostname "Use generic-lens or generic-optics with 'hostname' instead"  #-}

-- | The list of IP v4 addresses of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'ipv4Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaIpv4Addresses :: Lens.Lens' AssetAttributes (Core.Maybe [Types.Ipv4Address])
aaIpv4Addresses = Lens.field @"ipv4Addresses"
{-# INLINEABLE aaIpv4Addresses #-}
{-# DEPRECATED ipv4Addresses "Use generic-lens or generic-optics with 'ipv4Addresses' instead"  #-}

-- | An array of the network interfaces interacting with the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaNetworkInterfaces :: Lens.Lens' AssetAttributes (Core.Maybe [Types.NetworkInterface])
aaNetworkInterfaces = Lens.field @"networkInterfaces"
{-# INLINEABLE aaNetworkInterfaces #-}
{-# DEPRECATED networkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead"  #-}

-- | The tags related to the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaTags :: Lens.Lens' AssetAttributes (Core.Maybe [Types.Tag])
aaTags = Lens.field @"tags"
{-# INLINEABLE aaTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON AssetAttributes where
        parseJSON
          = Core.withObject "AssetAttributes" Core.$
              \ x ->
                AssetAttributes' Core.<$>
                  (x Core..: "schemaVersion") Core.<*> x Core..:? "agentId" Core.<*>
                    x Core..:? "amiId"
                    Core.<*> x Core..:? "autoScalingGroup"
                    Core.<*> x Core..:? "hostname"
                    Core.<*> x Core..:? "ipv4Addresses"
                    Core.<*> x Core..:? "networkInterfaces"
                    Core.<*> x Core..:? "tags"
