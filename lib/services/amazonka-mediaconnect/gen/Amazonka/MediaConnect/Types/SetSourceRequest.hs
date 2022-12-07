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
-- Module      : Amazonka.MediaConnect.Types.SetSourceRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.SetSourceRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.MediaStreamSourceConfigurationRequest
import Amazonka.MediaConnect.Types.Protocol
import qualified Amazonka.Prelude as Prelude

-- | The settings for the source of the flow.
--
-- /See:/ 'newSetSourceRequest' smart constructor.
data SetSourceRequest = SetSourceRequest'
  { -- | The maximum latency in milliseconds. This parameter applies only to
    -- RIST-based, Zixi-based, and Fujitsu-based streams.
    maxLatency :: Prelude.Maybe Prelude.Int,
    -- | The name of the source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The media streams that are associated with the source, and the
    -- parameters for those associations.
    mediaStreamSourceConfigurations :: Prelude.Maybe [MediaStreamSourceConfigurationRequest],
    -- | The ARN of the entitlement that allows you to subscribe to this flow.
    -- The entitlement is set by the flow originator, and the ARN is generated
    -- as part of the originator\'s flow.
    entitlementArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the VPC interface to use for this source.
    vpcInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The size of the buffer (in milliseconds) to use to sync incoming source
    -- data.
    maxSyncBuffer :: Prelude.Maybe Prelude.Int,
    -- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
    maxBitrate :: Prelude.Maybe Prelude.Int,
    -- | The stream ID that you want to use for this transport. This parameter
    -- applies only to Zixi-based streams.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The IP address that the flow communicates with to initiate connection
    -- with the sender.
    senderIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption that is used on the content ingested from this
    -- source.
    decryption :: Prelude.Maybe Encryption,
    -- | A description for the source. This value is not used or seen outside of
    -- the current AWS Elemental MediaConnect account.
    description :: Prelude.Maybe Prelude.Text,
    -- | Source IP or domain name for SRT-caller protocol.
    sourceListenerAddress :: Prelude.Maybe Prelude.Text,
    -- | The port that the flow uses to send outbound requests to initiate
    -- connection with the sender.
    senderControlPort :: Prelude.Maybe Prelude.Int,
    -- | The protocol that is used by the source.
    protocol :: Prelude.Maybe Protocol,
    -- | The port that the flow will be listening on for incoming content.
    ingestPort :: Prelude.Maybe Prelude.Int,
    -- | The range of IP addresses that should be allowed to contribute content
    -- to your source. These IP addresses should be in the form of a Classless
    -- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
    whitelistCidr :: Prelude.Maybe Prelude.Text,
    -- | The minimum latency in milliseconds for SRT-based streams. In streams
    -- that use the SRT protocol, this value that you set on your MediaConnect
    -- source or output represents the minimal potential latency of that
    -- connection. The latency of the stream is set to the highest number
    -- between the sender’s minimum latency and the receiver’s minimum latency.
    minLatency :: Prelude.Maybe Prelude.Int,
    -- | Source port for SRT-caller protocol.
    sourceListenerPort :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLatency', 'setSourceRequest_maxLatency' - The maximum latency in milliseconds. This parameter applies only to
-- RIST-based, Zixi-based, and Fujitsu-based streams.
--
-- 'name', 'setSourceRequest_name' - The name of the source.
--
-- 'mediaStreamSourceConfigurations', 'setSourceRequest_mediaStreamSourceConfigurations' - The media streams that are associated with the source, and the
-- parameters for those associations.
--
-- 'entitlementArn', 'setSourceRequest_entitlementArn' - The ARN of the entitlement that allows you to subscribe to this flow.
-- The entitlement is set by the flow originator, and the ARN is generated
-- as part of the originator\'s flow.
--
-- 'vpcInterfaceName', 'setSourceRequest_vpcInterfaceName' - The name of the VPC interface to use for this source.
--
-- 'maxSyncBuffer', 'setSourceRequest_maxSyncBuffer' - The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
--
-- 'maxBitrate', 'setSourceRequest_maxBitrate' - The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
--
-- 'streamId', 'setSourceRequest_streamId' - The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
--
-- 'senderIpAddress', 'setSourceRequest_senderIpAddress' - The IP address that the flow communicates with to initiate connection
-- with the sender.
--
-- 'decryption', 'setSourceRequest_decryption' - The type of encryption that is used on the content ingested from this
-- source.
--
-- 'description', 'setSourceRequest_description' - A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
--
-- 'sourceListenerAddress', 'setSourceRequest_sourceListenerAddress' - Source IP or domain name for SRT-caller protocol.
--
-- 'senderControlPort', 'setSourceRequest_senderControlPort' - The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
--
-- 'protocol', 'setSourceRequest_protocol' - The protocol that is used by the source.
--
-- 'ingestPort', 'setSourceRequest_ingestPort' - The port that the flow will be listening on for incoming content.
--
-- 'whitelistCidr', 'setSourceRequest_whitelistCidr' - The range of IP addresses that should be allowed to contribute content
-- to your source. These IP addresses should be in the form of a Classless
-- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'minLatency', 'setSourceRequest_minLatency' - The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
--
-- 'sourceListenerPort', 'setSourceRequest_sourceListenerPort' - Source port for SRT-caller protocol.
newSetSourceRequest ::
  SetSourceRequest
newSetSourceRequest =
  SetSourceRequest'
    { maxLatency = Prelude.Nothing,
      name = Prelude.Nothing,
      mediaStreamSourceConfigurations = Prelude.Nothing,
      entitlementArn = Prelude.Nothing,
      vpcInterfaceName = Prelude.Nothing,
      maxSyncBuffer = Prelude.Nothing,
      maxBitrate = Prelude.Nothing,
      streamId = Prelude.Nothing,
      senderIpAddress = Prelude.Nothing,
      decryption = Prelude.Nothing,
      description = Prelude.Nothing,
      sourceListenerAddress = Prelude.Nothing,
      senderControlPort = Prelude.Nothing,
      protocol = Prelude.Nothing,
      ingestPort = Prelude.Nothing,
      whitelistCidr = Prelude.Nothing,
      minLatency = Prelude.Nothing,
      sourceListenerPort = Prelude.Nothing
    }

-- | The maximum latency in milliseconds. This parameter applies only to
-- RIST-based, Zixi-based, and Fujitsu-based streams.
setSourceRequest_maxLatency :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_maxLatency = Lens.lens (\SetSourceRequest' {maxLatency} -> maxLatency) (\s@SetSourceRequest' {} a -> s {maxLatency = a} :: SetSourceRequest)

-- | The name of the source.
setSourceRequest_name :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_name = Lens.lens (\SetSourceRequest' {name} -> name) (\s@SetSourceRequest' {} a -> s {name = a} :: SetSourceRequest)

-- | The media streams that are associated with the source, and the
-- parameters for those associations.
setSourceRequest_mediaStreamSourceConfigurations :: Lens.Lens' SetSourceRequest (Prelude.Maybe [MediaStreamSourceConfigurationRequest])
setSourceRequest_mediaStreamSourceConfigurations = Lens.lens (\SetSourceRequest' {mediaStreamSourceConfigurations} -> mediaStreamSourceConfigurations) (\s@SetSourceRequest' {} a -> s {mediaStreamSourceConfigurations = a} :: SetSourceRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the entitlement that allows you to subscribe to this flow.
-- The entitlement is set by the flow originator, and the ARN is generated
-- as part of the originator\'s flow.
setSourceRequest_entitlementArn :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_entitlementArn = Lens.lens (\SetSourceRequest' {entitlementArn} -> entitlementArn) (\s@SetSourceRequest' {} a -> s {entitlementArn = a} :: SetSourceRequest)

-- | The name of the VPC interface to use for this source.
setSourceRequest_vpcInterfaceName :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_vpcInterfaceName = Lens.lens (\SetSourceRequest' {vpcInterfaceName} -> vpcInterfaceName) (\s@SetSourceRequest' {} a -> s {vpcInterfaceName = a} :: SetSourceRequest)

-- | The size of the buffer (in milliseconds) to use to sync incoming source
-- data.
setSourceRequest_maxSyncBuffer :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_maxSyncBuffer = Lens.lens (\SetSourceRequest' {maxSyncBuffer} -> maxSyncBuffer) (\s@SetSourceRequest' {} a -> s {maxSyncBuffer = a} :: SetSourceRequest)

-- | The smoothing max bitrate for RIST, RTP, and RTP-FEC streams.
setSourceRequest_maxBitrate :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_maxBitrate = Lens.lens (\SetSourceRequest' {maxBitrate} -> maxBitrate) (\s@SetSourceRequest' {} a -> s {maxBitrate = a} :: SetSourceRequest)

-- | The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
setSourceRequest_streamId :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_streamId = Lens.lens (\SetSourceRequest' {streamId} -> streamId) (\s@SetSourceRequest' {} a -> s {streamId = a} :: SetSourceRequest)

-- | The IP address that the flow communicates with to initiate connection
-- with the sender.
setSourceRequest_senderIpAddress :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_senderIpAddress = Lens.lens (\SetSourceRequest' {senderIpAddress} -> senderIpAddress) (\s@SetSourceRequest' {} a -> s {senderIpAddress = a} :: SetSourceRequest)

-- | The type of encryption that is used on the content ingested from this
-- source.
setSourceRequest_decryption :: Lens.Lens' SetSourceRequest (Prelude.Maybe Encryption)
setSourceRequest_decryption = Lens.lens (\SetSourceRequest' {decryption} -> decryption) (\s@SetSourceRequest' {} a -> s {decryption = a} :: SetSourceRequest)

-- | A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
setSourceRequest_description :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_description = Lens.lens (\SetSourceRequest' {description} -> description) (\s@SetSourceRequest' {} a -> s {description = a} :: SetSourceRequest)

-- | Source IP or domain name for SRT-caller protocol.
setSourceRequest_sourceListenerAddress :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_sourceListenerAddress = Lens.lens (\SetSourceRequest' {sourceListenerAddress} -> sourceListenerAddress) (\s@SetSourceRequest' {} a -> s {sourceListenerAddress = a} :: SetSourceRequest)

-- | The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
setSourceRequest_senderControlPort :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_senderControlPort = Lens.lens (\SetSourceRequest' {senderControlPort} -> senderControlPort) (\s@SetSourceRequest' {} a -> s {senderControlPort = a} :: SetSourceRequest)

-- | The protocol that is used by the source.
setSourceRequest_protocol :: Lens.Lens' SetSourceRequest (Prelude.Maybe Protocol)
setSourceRequest_protocol = Lens.lens (\SetSourceRequest' {protocol} -> protocol) (\s@SetSourceRequest' {} a -> s {protocol = a} :: SetSourceRequest)

-- | The port that the flow will be listening on for incoming content.
setSourceRequest_ingestPort :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_ingestPort = Lens.lens (\SetSourceRequest' {ingestPort} -> ingestPort) (\s@SetSourceRequest' {} a -> s {ingestPort = a} :: SetSourceRequest)

-- | The range of IP addresses that should be allowed to contribute content
-- to your source. These IP addresses should be in the form of a Classless
-- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
setSourceRequest_whitelistCidr :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Text)
setSourceRequest_whitelistCidr = Lens.lens (\SetSourceRequest' {whitelistCidr} -> whitelistCidr) (\s@SetSourceRequest' {} a -> s {whitelistCidr = a} :: SetSourceRequest)

-- | The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
setSourceRequest_minLatency :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_minLatency = Lens.lens (\SetSourceRequest' {minLatency} -> minLatency) (\s@SetSourceRequest' {} a -> s {minLatency = a} :: SetSourceRequest)

-- | Source port for SRT-caller protocol.
setSourceRequest_sourceListenerPort :: Lens.Lens' SetSourceRequest (Prelude.Maybe Prelude.Int)
setSourceRequest_sourceListenerPort = Lens.lens (\SetSourceRequest' {sourceListenerPort} -> sourceListenerPort) (\s@SetSourceRequest' {} a -> s {sourceListenerPort = a} :: SetSourceRequest)

instance Prelude.Hashable SetSourceRequest where
  hashWithSalt _salt SetSourceRequest' {..} =
    _salt `Prelude.hashWithSalt` maxLatency
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` mediaStreamSourceConfigurations
      `Prelude.hashWithSalt` entitlementArn
      `Prelude.hashWithSalt` vpcInterfaceName
      `Prelude.hashWithSalt` maxSyncBuffer
      `Prelude.hashWithSalt` maxBitrate
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` senderIpAddress
      `Prelude.hashWithSalt` decryption
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceListenerAddress
      `Prelude.hashWithSalt` senderControlPort
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` ingestPort
      `Prelude.hashWithSalt` whitelistCidr
      `Prelude.hashWithSalt` minLatency
      `Prelude.hashWithSalt` sourceListenerPort

instance Prelude.NFData SetSourceRequest where
  rnf SetSourceRequest' {..} =
    Prelude.rnf maxLatency
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf mediaStreamSourceConfigurations
      `Prelude.seq` Prelude.rnf entitlementArn
      `Prelude.seq` Prelude.rnf vpcInterfaceName
      `Prelude.seq` Prelude.rnf maxSyncBuffer
      `Prelude.seq` Prelude.rnf maxBitrate
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf senderIpAddress
      `Prelude.seq` Prelude.rnf decryption
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sourceListenerAddress
      `Prelude.seq` Prelude.rnf senderControlPort
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf ingestPort
      `Prelude.seq` Prelude.rnf whitelistCidr
      `Prelude.seq` Prelude.rnf minLatency
      `Prelude.seq` Prelude.rnf sourceListenerPort

instance Data.ToJSON SetSourceRequest where
  toJSON SetSourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxLatency" Data..=) Prelude.<$> maxLatency,
            ("name" Data..=) Prelude.<$> name,
            ("mediaStreamSourceConfigurations" Data..=)
              Prelude.<$> mediaStreamSourceConfigurations,
            ("entitlementArn" Data..=)
              Prelude.<$> entitlementArn,
            ("vpcInterfaceName" Data..=)
              Prelude.<$> vpcInterfaceName,
            ("maxSyncBuffer" Data..=) Prelude.<$> maxSyncBuffer,
            ("maxBitrate" Data..=) Prelude.<$> maxBitrate,
            ("streamId" Data..=) Prelude.<$> streamId,
            ("senderIpAddress" Data..=)
              Prelude.<$> senderIpAddress,
            ("decryption" Data..=) Prelude.<$> decryption,
            ("description" Data..=) Prelude.<$> description,
            ("sourceListenerAddress" Data..=)
              Prelude.<$> sourceListenerAddress,
            ("senderControlPort" Data..=)
              Prelude.<$> senderControlPort,
            ("protocol" Data..=) Prelude.<$> protocol,
            ("ingestPort" Data..=) Prelude.<$> ingestPort,
            ("whitelistCidr" Data..=) Prelude.<$> whitelistCidr,
            ("minLatency" Data..=) Prelude.<$> minLatency,
            ("sourceListenerPort" Data..=)
              Prelude.<$> sourceListenerPort
          ]
      )
