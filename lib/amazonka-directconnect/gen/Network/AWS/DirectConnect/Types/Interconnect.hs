-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Interconnect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Interconnect
  ( Interconnect (..),

    -- * Smart constructor
    mkInterconnect,

    -- * Lenses
    iLagId,
    iInterconnectId,
    iLocation,
    iInterconnectName,
    iAwsDevice,
    iHasLogicalRedundancy,
    iLoaIssueTime,
    iBandwidth,
    iJumboFrameCapable,
    iInterconnectState,
    iRegion,
    iProviderName,
    iAwsDeviceV2,
    iTags,
  )
where

import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.InterconnectState
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an interconnect.
--
-- /See:/ 'mkInterconnect' smart constructor.
data Interconnect = Interconnect'
  { lagId :: Lude.Maybe Lude.Text,
    interconnectId :: Lude.Maybe Lude.Text,
    location :: Lude.Maybe Lude.Text,
    interconnectName :: Lude.Maybe Lude.Text,
    awsDevice :: Lude.Maybe Lude.Text,
    hasLogicalRedundancy :: Lude.Maybe HasLogicalRedundancy,
    loaIssueTime :: Lude.Maybe Lude.Timestamp,
    bandwidth :: Lude.Maybe Lude.Text,
    jumboFrameCapable :: Lude.Maybe Lude.Bool,
    interconnectState :: Lude.Maybe InterconnectState,
    region :: Lude.Maybe Lude.Text,
    providerName :: Lude.Maybe Lude.Text,
    awsDeviceV2 :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.NonEmpty Tag)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Interconnect' with the minimum fields required to make a request.
--
-- * 'awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
-- * 'awsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
-- * 'bandwidth' - The bandwidth of the connection.
-- * 'hasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
-- * 'interconnectId' - The ID of the interconnect.
-- * 'interconnectName' - The name of the interconnect.
-- * 'interconnectState' - The state of the interconnect. The following are the possible values:
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
-- * 'jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
-- * 'lagId' - The ID of the LAG.
-- * 'loaIssueTime' - The time of the most recent call to 'DescribeLoa' for this connection.
-- * 'location' - The location of the connection.
-- * 'providerName' - The name of the service provider associated with the interconnect.
-- * 'region' - The AWS Region where the connection is located.
-- * 'tags' - The tags associated with the interconnect.
mkInterconnect ::
  Interconnect
mkInterconnect =
  Interconnect'
    { lagId = Lude.Nothing,
      interconnectId = Lude.Nothing,
      location = Lude.Nothing,
      interconnectName = Lude.Nothing,
      awsDevice = Lude.Nothing,
      hasLogicalRedundancy = Lude.Nothing,
      loaIssueTime = Lude.Nothing,
      bandwidth = Lude.Nothing,
      jumboFrameCapable = Lude.Nothing,
      interconnectState = Lude.Nothing,
      region = Lude.Nothing,
      providerName = Lude.Nothing,
      awsDeviceV2 = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the LAG.
--
-- /Note:/ Consider using 'lagId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLagId :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iLagId = Lens.lens (lagId :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {lagId = a} :: Interconnect)
{-# DEPRECATED iLagId "Use generic-lens or generic-optics with 'lagId' instead." #-}

-- | The ID of the interconnect.
--
-- /Note:/ Consider using 'interconnectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInterconnectId :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iInterconnectId = Lens.lens (interconnectId :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {interconnectId = a} :: Interconnect)
{-# DEPRECATED iInterconnectId "Use generic-lens or generic-optics with 'interconnectId' instead." #-}

-- | The location of the connection.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLocation :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iLocation = Lens.lens (location :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: Interconnect)
{-# DEPRECATED iLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the interconnect.
--
-- /Note:/ Consider using 'interconnectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInterconnectName :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iInterconnectName = Lens.lens (interconnectName :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {interconnectName = a} :: Interconnect)
{-# DEPRECATED iInterconnectName "Use generic-lens or generic-optics with 'interconnectName' instead." #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAwsDevice :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iAwsDevice = Lens.lens (awsDevice :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {awsDevice = a} :: Interconnect)
{-# DEPRECATED iAwsDevice "Use generic-lens or generic-optics with 'awsDevice' instead." #-}

-- | Indicates whether the interconnect supports a secondary BGP in the same address family (IPv4/IPv6).
--
-- /Note:/ Consider using 'hasLogicalRedundancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHasLogicalRedundancy :: Lens.Lens' Interconnect (Lude.Maybe HasLogicalRedundancy)
iHasLogicalRedundancy = Lens.lens (hasLogicalRedundancy :: Interconnect -> Lude.Maybe HasLogicalRedundancy) (\s a -> s {hasLogicalRedundancy = a} :: Interconnect)
{-# DEPRECATED iHasLogicalRedundancy "Use generic-lens or generic-optics with 'hasLogicalRedundancy' instead." #-}

-- | The time of the most recent call to 'DescribeLoa' for this connection.
--
-- /Note:/ Consider using 'loaIssueTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLoaIssueTime :: Lens.Lens' Interconnect (Lude.Maybe Lude.Timestamp)
iLoaIssueTime = Lens.lens (loaIssueTime :: Interconnect -> Lude.Maybe Lude.Timestamp) (\s a -> s {loaIssueTime = a} :: Interconnect)
{-# DEPRECATED iLoaIssueTime "Use generic-lens or generic-optics with 'loaIssueTime' instead." #-}

-- | The bandwidth of the connection.
--
-- /Note:/ Consider using 'bandwidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBandwidth :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iBandwidth = Lens.lens (bandwidth :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {bandwidth = a} :: Interconnect)
{-# DEPRECATED iBandwidth "Use generic-lens or generic-optics with 'bandwidth' instead." #-}

-- | Indicates whether jumbo frames (9001 MTU) are supported.
--
-- /Note:/ Consider using 'jumboFrameCapable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iJumboFrameCapable :: Lens.Lens' Interconnect (Lude.Maybe Lude.Bool)
iJumboFrameCapable = Lens.lens (jumboFrameCapable :: Interconnect -> Lude.Maybe Lude.Bool) (\s a -> s {jumboFrameCapable = a} :: Interconnect)
{-# DEPRECATED iJumboFrameCapable "Use generic-lens or generic-optics with 'jumboFrameCapable' instead." #-}

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
iInterconnectState :: Lens.Lens' Interconnect (Lude.Maybe InterconnectState)
iInterconnectState = Lens.lens (interconnectState :: Interconnect -> Lude.Maybe InterconnectState) (\s a -> s {interconnectState = a} :: Interconnect)
{-# DEPRECATED iInterconnectState "Use generic-lens or generic-optics with 'interconnectState' instead." #-}

-- | The AWS Region where the connection is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRegion :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iRegion = Lens.lens (region :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: Interconnect)
{-# DEPRECATED iRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The name of the service provider associated with the interconnect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iProviderName :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iProviderName = Lens.lens (providerName :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: Interconnect)
{-# DEPRECATED iProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The Direct Connect endpoint on which the physical connection terminates.
--
-- /Note:/ Consider using 'awsDeviceV2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAwsDeviceV2 :: Lens.Lens' Interconnect (Lude.Maybe Lude.Text)
iAwsDeviceV2 = Lens.lens (awsDeviceV2 :: Interconnect -> Lude.Maybe Lude.Text) (\s a -> s {awsDeviceV2 = a} :: Interconnect)
{-# DEPRECATED iAwsDeviceV2 "Use generic-lens or generic-optics with 'awsDeviceV2' instead." #-}

-- | The tags associated with the interconnect.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Interconnect (Lude.Maybe (Lude.NonEmpty Tag))
iTags = Lens.lens (tags :: Interconnect -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: Interconnect)
{-# DEPRECATED iTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Interconnect where
  parseJSON =
    Lude.withObject
      "Interconnect"
      ( \x ->
          Interconnect'
            Lude.<$> (x Lude..:? "lagId")
            Lude.<*> (x Lude..:? "interconnectId")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "interconnectName")
            Lude.<*> (x Lude..:? "awsDevice")
            Lude.<*> (x Lude..:? "hasLogicalRedundancy")
            Lude.<*> (x Lude..:? "loaIssueTime")
            Lude.<*> (x Lude..:? "bandwidth")
            Lude.<*> (x Lude..:? "jumboFrameCapable")
            Lude.<*> (x Lude..:? "interconnectState")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "providerName")
            Lude.<*> (x Lude..:? "awsDeviceV2")
            Lude.<*> (x Lude..:? "tags")
      )
