{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssetAttributes
  ( AssetAttributes (..),

    -- * Smart constructor
    mkAssetAttributes,

    -- * Lenses
    aaHostname,
    aaAutoScalingGroup,
    aaNetworkInterfaces,
    aaIpv4Addresses,
    aaAgentId,
    aaAmiId,
    aaTags,
    aaSchemaVersion,
  )
where

import Network.AWS.Inspector.Types.NetworkInterface
import Network.AWS.Inspector.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of attributes of the host from which the finding is generated.
--
-- /See:/ 'mkAssetAttributes' smart constructor.
data AssetAttributes = AssetAttributes'
  { hostname ::
      Lude.Maybe Lude.Text,
    autoScalingGroup :: Lude.Maybe Lude.Text,
    networkInterfaces :: Lude.Maybe [NetworkInterface],
    ipv4Addresses :: Lude.Maybe [Lude.Text],
    agentId :: Lude.Maybe Lude.Text,
    amiId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    schemaVersion :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssetAttributes' with the minimum fields required to make a request.
--
-- * 'agentId' - The ID of the agent that is installed on the EC2 instance where the finding is generated.
-- * 'amiId' - The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
-- * 'autoScalingGroup' - The Auto Scaling group of the EC2 instance where the finding is generated.
-- * 'hostname' - The hostname of the EC2 instance where the finding is generated.
-- * 'ipv4Addresses' - The list of IP v4 addresses of the EC2 instance where the finding is generated.
-- * 'networkInterfaces' - An array of the network interfaces interacting with the EC2 instance where the finding is generated.
-- * 'schemaVersion' - The schema version of this data type.
-- * 'tags' - The tags related to the EC2 instance where the finding is generated.
mkAssetAttributes ::
  -- | 'schemaVersion'
  Lude.Natural ->
  AssetAttributes
mkAssetAttributes pSchemaVersion_ =
  AssetAttributes'
    { hostname = Lude.Nothing,
      autoScalingGroup = Lude.Nothing,
      networkInterfaces = Lude.Nothing,
      ipv4Addresses = Lude.Nothing,
      agentId = Lude.Nothing,
      amiId = Lude.Nothing,
      tags = Lude.Nothing,
      schemaVersion = pSchemaVersion_
    }

-- | The hostname of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaHostname :: Lens.Lens' AssetAttributes (Lude.Maybe Lude.Text)
aaHostname = Lens.lens (hostname :: AssetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: AssetAttributes)
{-# DEPRECATED aaHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The Auto Scaling group of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'autoScalingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAutoScalingGroup :: Lens.Lens' AssetAttributes (Lude.Maybe Lude.Text)
aaAutoScalingGroup = Lens.lens (autoScalingGroup :: AssetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroup = a} :: AssetAttributes)
{-# DEPRECATED aaAutoScalingGroup "Use generic-lens or generic-optics with 'autoScalingGroup' instead." #-}

-- | An array of the network interfaces interacting with the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'networkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaNetworkInterfaces :: Lens.Lens' AssetAttributes (Lude.Maybe [NetworkInterface])
aaNetworkInterfaces = Lens.lens (networkInterfaces :: AssetAttributes -> Lude.Maybe [NetworkInterface]) (\s a -> s {networkInterfaces = a} :: AssetAttributes)
{-# DEPRECATED aaNetworkInterfaces "Use generic-lens or generic-optics with 'networkInterfaces' instead." #-}

-- | The list of IP v4 addresses of the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'ipv4Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaIpv4Addresses :: Lens.Lens' AssetAttributes (Lude.Maybe [Lude.Text])
aaIpv4Addresses = Lens.lens (ipv4Addresses :: AssetAttributes -> Lude.Maybe [Lude.Text]) (\s a -> s {ipv4Addresses = a} :: AssetAttributes)
{-# DEPRECATED aaIpv4Addresses "Use generic-lens or generic-optics with 'ipv4Addresses' instead." #-}

-- | The ID of the agent that is installed on the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'agentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAgentId :: Lens.Lens' AssetAttributes (Lude.Maybe Lude.Text)
aaAgentId = Lens.lens (agentId :: AssetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {agentId = a} :: AssetAttributes)
{-# DEPRECATED aaAgentId "Use generic-lens or generic-optics with 'agentId' instead." #-}

-- | The ID of the Amazon Machine Image (AMI) that is installed on the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAmiId :: Lens.Lens' AssetAttributes (Lude.Maybe Lude.Text)
aaAmiId = Lens.lens (amiId :: AssetAttributes -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: AssetAttributes)
{-# DEPRECATED aaAmiId "Use generic-lens or generic-optics with 'amiId' instead." #-}

-- | The tags related to the EC2 instance where the finding is generated.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaTags :: Lens.Lens' AssetAttributes (Lude.Maybe [Tag])
aaTags = Lens.lens (tags :: AssetAttributes -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: AssetAttributes)
{-# DEPRECATED aaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaSchemaVersion :: Lens.Lens' AssetAttributes Lude.Natural
aaSchemaVersion = Lens.lens (schemaVersion :: AssetAttributes -> Lude.Natural) (\s a -> s {schemaVersion = a} :: AssetAttributes)
{-# DEPRECATED aaSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

instance Lude.FromJSON AssetAttributes where
  parseJSON =
    Lude.withObject
      "AssetAttributes"
      ( \x ->
          AssetAttributes'
            Lude.<$> (x Lude..:? "hostname")
            Lude.<*> (x Lude..:? "autoScalingGroup")
            Lude.<*> (x Lude..:? "networkInterfaces" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ipv4Addresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "agentId")
            Lude.<*> (x Lude..:? "amiId")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "schemaVersion")
      )
