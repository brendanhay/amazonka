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
-- Module      : Amazonka.MediaConnect.Types.Transport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Transport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | Attributes related to the transport stream that are used in a source or
-- output.
--
-- /See:/ 'newTransport' smart constructor.
data Transport = Transport'
  { -- | The maximum latency in milliseconds. This parameter applies only to
    -- RIST-based, Zixi-based, and Fujitsu-based streams.
    maxLatency :: Prelude.Maybe Prelude.Int,
    -- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
    -- streams.
    smoothingLatency :: Prelude.Maybe Prelude.Int,
    -- | The size of the buffer (in milliseconds) to use to sync incoming source
    -- data.
    maxSyncBuffer :: Prelude.Maybe Prelude.Int,
    -- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The range of IP addresses that should be allowed to initiate output
    -- requests to this flow. These IP addresses should be in the form of a
    -- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
    cidrAllowList :: Prelude.Maybe [Prelude.Text],
    -- | The stream ID that you want to use for this transport. This parameter
    -- applies only to Zixi-based streams.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The remote ID for the Zixi-pull stream.
    remoteId :: Prelude.Maybe Prelude.Text,
    -- | The IP address that the flow communicates with to initiate connection
    -- with the sender.
    senderIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Source IP or domain name for SRT-caller protocol.
    sourceListenerAddress :: Prelude.Maybe Prelude.Text,
    -- | The port that the flow uses to send outbound requests to initiate
    -- connection with the sender.
    senderControlPort :: Prelude.Maybe Prelude.Int,
    -- | The minimum latency in milliseconds for SRT-based streams. In streams
    -- that use the SRT protocol, this value that you set on your MediaConnect
    -- source or output represents the minimal potential latency of that
    -- connection. The latency of the stream is set to the highest number
    -- between the sender’s minimum latency and the receiver’s minimum latency.
    minLatency :: Prelude.Maybe Prelude.Int,
    -- | Source port for SRT-caller protocol.
    sourceListenerPort :: Prelude.Maybe Prelude.Int,
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
-- RIST-based, Zixi-based, and Fujitsu-based streams.
--
-- 'smoothingLatency', 'transport_smoothingLatency' - The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
--
-- 'maxSyncBuffer', 'transport_maxSyncBuffer' - The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
--
-- 'maxBitrate', 'transport_maxBitrate' - The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
--
-- 'cidrAllowList', 'transport_cidrAllowList' - The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'streamId', 'transport_streamId' - The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
--
-- 'remoteId', 'transport_remoteId' - The remote ID for the Zixi-pull stream.
--
-- 'senderIpAddress', 'transport_senderIpAddress' - The IP address that the flow communicates with to initiate connection
-- with the sender.
--
-- 'sourceListenerAddress', 'transport_sourceListenerAddress' - Source IP or domain name for SRT-caller protocol.
--
-- 'senderControlPort', 'transport_senderControlPort' - The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
--
-- 'minLatency', 'transport_minLatency' - The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
--
-- 'sourceListenerPort', 'transport_sourceListenerPort' - Source port for SRT-caller protocol.
--
-- 'protocol', 'transport_protocol' - The protocol that is used by the source or output.
newTransport ::
  -- | 'protocol'
  Protocol ->
  Transport
newTransport pProtocol_ =
  Transport'
    { maxLatency = Prelude.Nothing,
      smoothingLatency = Prelude.Nothing,
      maxSyncBuffer = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      cidrAllowList = Prelude.Nothing,
      streamId = Prelude.Nothing,
      remoteId = Prelude.Nothing,
      senderIpAddress = Prelude.Nothing,
      sourceListenerAddress = Prelude.Nothing,
      senderControlPort = Prelude.Nothing,
      minLatency = Prelude.Nothing,
      sourceListenerPort = Prelude.Nothing,
      protocol = pProtocol_
    }

-- | The maximum latency in milliseconds. This parameter applies only to
-- RIST-based, Zixi-based, and Fujitsu-based streams.
transport_maxLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxLatency = Lens.lens (\Transport' {maxLatency} -> maxLatency) (\s@Transport' {} a -> s {maxLatency = a} :: Transport)

-- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
transport_smoothingLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_smoothingLatency = Lens.lens (\Transport' {smoothingLatency} -> smoothingLatency) (\s@Transport' {} a -> s {smoothingLatency = a} :: Transport)

-- | The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
transport_maxSyncBuffer :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxSyncBuffer = Lens.lens (\Transport' {maxSyncBuffer} -> maxSyncBuffer) (\s@Transport' {} a -> s {maxSyncBuffer = a} :: Transport)

-- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
transport_maxBitrate :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_maxBitrate = Lens.lens (\Transport' {maxBitrate} -> maxBitrate) (\s@Transport' {} a -> s {maxBitrate = a} :: Transport)

-- | The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
transport_cidrAllowList :: Lens.Lens' Transport (Prelude.Maybe [Prelude.Text])
transport_cidrAllowList = Lens.lens (\Transport' {cidrAllowList} -> cidrAllowList) (\s@Transport' {} a -> s {cidrAllowList = a} :: Transport) Prelude.. Lens.mapping Lens.coerced

-- | The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
transport_streamId :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_streamId = Lens.lens (\Transport' {streamId} -> streamId) (\s@Transport' {} a -> s {streamId = a} :: Transport)

-- | The remote ID for the Zixi-pull stream.
transport_remoteId :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_remoteId = Lens.lens (\Transport' {remoteId} -> remoteId) (\s@Transport' {} a -> s {remoteId = a} :: Transport)

-- | The IP address that the flow communicates with to initiate connection
-- with the sender.
transport_senderIpAddress :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_senderIpAddress = Lens.lens (\Transport' {senderIpAddress} -> senderIpAddress) (\s@Transport' {} a -> s {senderIpAddress = a} :: Transport)

-- | Source IP or domain name for SRT-caller protocol.
transport_sourceListenerAddress :: Lens.Lens' Transport (Prelude.Maybe Prelude.Text)
transport_sourceListenerAddress = Lens.lens (\Transport' {sourceListenerAddress} -> sourceListenerAddress) (\s@Transport' {} a -> s {sourceListenerAddress = a} :: Transport)

-- | The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
transport_senderControlPort :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_senderControlPort = Lens.lens (\Transport' {senderControlPort} -> senderControlPort) (\s@Transport' {} a -> s {senderControlPort = a} :: Transport)

-- | The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
transport_minLatency :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_minLatency = Lens.lens (\Transport' {minLatency} -> minLatency) (\s@Transport' {} a -> s {minLatency = a} :: Transport)

-- | Source port for SRT-caller protocol.
transport_sourceListenerPort :: Lens.Lens' Transport (Prelude.Maybe Prelude.Int)
transport_sourceListenerPort = Lens.lens (\Transport' {sourceListenerPort} -> sourceListenerPort) (\s@Transport' {} a -> s {sourceListenerPort = a} :: Transport)

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
            Prelude.<*> (x Core..:? "smoothingLatency")
            Prelude.<*> (x Core..:? "maxSyncBuffer")
            Prelude.<*> (x Core..:? "maxBitrate")
            Prelude.<*> (x Core..:? "cidrAllowList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "streamId")
            Prelude.<*> (x Core..:? "remoteId")
            Prelude.<*> (x Core..:? "senderIpAddress")
            Prelude.<*> (x Core..:? "sourceListenerAddress")
            Prelude.<*> (x Core..:? "senderControlPort")
            Prelude.<*> (x Core..:? "minLatency")
            Prelude.<*> (x Core..:? "sourceListenerPort")
            Prelude.<*> (x Core..: "protocol")
      )

instance Prelude.Hashable Transport where
  hashWithSalt _salt Transport' {..} =
    _salt `Prelude.hashWithSalt` maxLatency
      `Prelude.hashWithSalt` smoothingLatency
      `Prelude.hashWithSalt` maxSyncBuffer
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` cidrAllowList
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` remoteId
      `Prelude.hashWithSalt` senderIpAddress
      `Prelude.hashWithSalt` sourceListenerAddress
      `Prelude.hashWithSalt` senderControlPort
      `Prelude.hashWithSalt` minLatency
      `Prelude.hashWithSalt` sourceListenerPort
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData Transport where
  rnf Transport' {..} =
    Prelude.rnf maxLatency
      `Prelude.seq` Prelude.rnf smoothingLatency
      `Prelude.seq` Prelude.rnf maxSyncBuffer
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf cidrAllowList
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf remoteId
      `Prelude.seq` Prelude.rnf senderIpAddress
      `Prelude.seq` Prelude.rnf sourceListenerAddress
      `Prelude.seq` Prelude.rnf senderControlPort
      `Prelude.seq` Prelude.rnf minLatency
      `Prelude.seq` Prelude.rnf sourceListenerPort
      `Prelude.seq` Prelude.rnf protocol
