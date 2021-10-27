{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConnect.Types.Transport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConnect.Types.Transport where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConnect.Types.Protocol
import qualified Network.AWS.Prelude as Prelude

-- | Attributes related to the transport stream that are used in a source or
-- output.
--
-- /See:/ 'newTransport' smart constructor.
data Transport = Transport'
  { -- | The maximum latency in milliseconds. This parameter applies only to
    -- RIST-based and Zixi-based streams.
    maxLatency :: Prelude.Maybe Prelude.Int,
    -- | The size of the buffer (in milliseconds) to use to sync incoming source
    -- data.
    maxSyncBuffer :: Prelude.Maybe Prelude.Int,
    -- | The range of IP addresses that should be allowed to initiate output
    -- requests to this flow. These IP addresses should be in the form of a
    -- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
    cidrAllowList :: Prelude.Maybe [Prelude.Text],
    -- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
    -- streams.
    smoothingLatency :: Prelude.Maybe Prelude.Int,
    -- | The minimum latency in milliseconds for SRT-based streams. In streams
    -- that use the SRT protocol, this value that you set on your MediaConnect
    -- source or output represents the minimal potential latency of that
    -- connection. The latency of the stream is set to the highest number
    -- between the sender’s minimum latency and the receiver’s minimum latency.
    minLatency :: Prelude.Maybe Prelude.Int,
    -- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The stream ID that you want to use for this transport. This parameter
    -- applies only to Zixi-based streams.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The remote ID for the Zixi-pull stream.
    remoteId :: Prelude.Maybe Prelude.Text,
    -- | The protocol that is used by the source or output.
    protocol :: Protocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Transport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLatency', 'transport_maxLatency' - The maximum latency in milliseconds. This parameter applies only to
-- RIST-based and Zixi-based streams.
--
-- 'maxSyncBuffer', 'transport_maxSyncBuffer' - The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
--
-- 'cidrAllowList', 'transport_cidrAllowList' - The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'smoothingLatency', 'transport_smoothingLatency' - The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
--
-- 'minLatency', 'transport_minLatency' - The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
--
-- 'maxBitrate', 'transport_maxBitrate' - The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
--
-- 'streamId', 'transport_streamId' - The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
--
-- 'remoteId', 'transport_remoteId' - The remote ID for the Zixi-pull stream.
--
-- 'protocol', 'transport_protocol' - The protocol that is used by the source or output.
newTransport ::
  -- | 'protocol'
  Protocol ->
  Transport
newTransport pProtocol_ =
  Transport'
    { maxLatency = Prelude.Nothing,
      maxSyncBuffer = Prelude.Nothing,
      cidrAllowList = Prelude.Nothing,
      smoothingLatency = Prelude.Nothing,
      minLatency = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      streamId = Prelude.Nothing,
      remoteId = Prelude.Nothing,
      protocol = pProtocol_
    }

-- | The maximum latency in milliseconds. This parameter applies only to
-- RIST-based and Zixi-based streams.
transport_maxLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxLatency = Lens.lens (\Transport' {maxLatency} -> maxLatency) (\s@Transport' {} a -> s {maxLatency = a} :: Transport)

-- | The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
transport_maxSyncBuffer :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxSyncBuffer = Lens.lens (\Transport' {maxSyncBuffer} -> maxSyncBuffer) (\s@Transport' {} a -> s {maxSyncBuffer = a} :: Transport)

-- | The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
transport_cidrAllowList :: Lens.Lens' Transport (Prelude.Maybe [Prelude.Text])
transport_cidrAllowList = Lens.lens (\Transport' {cidrAllowList} -> cidrAllowList) (\s@Transport' {} a -> s {cidrAllowList = a} :: Transport) Prelude.. Lens.mapping Lens.coerced

-- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
transport_smoothingLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_smoothingLatency = Lens.lens (\Transport' {smoothingLatency} -> smoothingLatency) (\s@Transport' {} a -> s {smoothingLatency = a} :: Transport)

-- | The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
transport_minLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_minLatency = Lens.lens (\Transport' {minLatency} -> minLatency) (\s@Transport' {} a -> s {minLatency = a} :: Transport)

-- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
transport_maxBitrate :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxBitrate = Lens.lens (\Transport' {maxBitrate} -> maxBitrate) (\s@Transport' {} a -> s {maxBitrate = a} :: Transport)

-- | The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
transport_streamId :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_streamId = Lens.lens (\Transport' {streamId} -> streamId) (\s@Transport' {} a -> s {streamId = a} :: Transport)

-- | The remote ID for the Zixi-pull stream.
transport_remoteId :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_remoteId = Lens.lens (\Transport' {remoteId} -> remoteId) (\s@Transport' {} a -> s {remoteId = a} :: Transport)

-- | The protocol that is used by the source or output.
transport_protocol :: Lens.Lens' Transport Protocol
transport_protocol = Lens.lens (\Transport' {protocol} -> protocol) (\s@Transport' {} a -> s {protocol = a} :: Transport)

instance Core.FromJSON Transport where
  parseJSON =
    Core.withObject
      "Transport"
      ( \x ->
          Transport'
            Prelude.<$> (x Core..:? "maxLatency")
            Prelude.<*> (x Core..:? "maxSyncBuffer")
            Prelude.<*> (x Core..:? "cidrAllowList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "smoothingLatency")
            Prelude.<*> (x Core..:? "minLatency")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "remoteId")
            Prelude.<*> (x Core..: "protocol")
      )

instance Prelude.Hashable Transport

instance Prelude.NFData Transport
