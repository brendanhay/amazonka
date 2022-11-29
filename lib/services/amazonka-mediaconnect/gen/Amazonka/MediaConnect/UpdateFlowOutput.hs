{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.UpdateFlowOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing flow output.
module Amazonka.MediaConnect.UpdateFlowOutput
  ( -- * Creating a Request
    UpdateFlowOutput (..),
    newUpdateFlowOutput,

    -- * Request Lenses
    updateFlowOutput_destination,
    updateFlowOutput_port,
    updateFlowOutput_maxLatency,
    updateFlowOutput_mediaStreamOutputConfigurations,
    updateFlowOutput_smoothingLatency,
    updateFlowOutput_cidrAllowList,
    updateFlowOutput_streamId,
    updateFlowOutput_remoteId,
    updateFlowOutput_senderIpAddress,
    updateFlowOutput_vpcInterfaceAttachment,
    updateFlowOutput_description,
    updateFlowOutput_senderControlPort,
    updateFlowOutput_encryption,
    updateFlowOutput_protocol,
    updateFlowOutput_minLatency,
    updateFlowOutput_flowArn,
    updateFlowOutput_outputArn,

    -- * Destructuring the Response
    UpdateFlowOutputResponse (..),
    newUpdateFlowOutputResponse,

    -- * Response Lenses
    updateFlowOutputResponse_output,
    updateFlowOutputResponse_flowArn,
    updateFlowOutputResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The fields that you want to update in the output.
--
-- /See:/ 'newUpdateFlowOutput' smart constructor.
data UpdateFlowOutput = UpdateFlowOutput'
  { -- | The IP address where you want to send the output.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The port to use when content is distributed to this output.
    port :: Prelude.Maybe Prelude.Int,
    -- | The maximum latency in milliseconds. This parameter applies only to
    -- RIST-based, Zixi-based, and Fujitsu-based streams.
    maxLatency :: Prelude.Maybe Prelude.Int,
    -- | The media streams that are associated with the output, and the
    -- parameters for those associations.
    mediaStreamOutputConfigurations :: Prelude.Maybe [MediaStreamOutputConfigurationRequest],
    -- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
    -- streams.
    smoothingLatency :: Prelude.Maybe Prelude.Int,
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
    -- | The name of the VPC interface attachment to use for this output.
    vpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | A description of the output. This description appears only on the AWS
    -- Elemental MediaConnect console and will not be seen by the end user.
    description :: Prelude.Maybe Prelude.Text,
    -- | The port that the flow uses to send outbound requests to initiate
    -- connection with the sender.
    senderControlPort :: Prelude.Maybe Prelude.Int,
    -- | The type of key used for the encryption. If no keyType is provided, the
    -- service will use the default setting (static-key).
    encryption :: Prelude.Maybe UpdateEncryption,
    -- | The protocol to use for the output.
    protocol :: Prelude.Maybe Protocol,
    -- | The minimum latency in milliseconds for SRT-based streams. In streams
    -- that use the SRT protocol, this value that you set on your MediaConnect
    -- source or output represents the minimal potential latency of that
    -- connection. The latency of the stream is set to the highest number
    -- between the sender’s minimum latency and the receiver’s minimum latency.
    minLatency :: Prelude.Maybe Prelude.Int,
    -- | The flow that is associated with the output that you want to update.
    flowArn :: Prelude.Text,
    -- | The ARN of the output that you want to update.
    outputArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'updateFlowOutput_destination' - The IP address where you want to send the output.
--
-- 'port', 'updateFlowOutput_port' - The port to use when content is distributed to this output.
--
-- 'maxLatency', 'updateFlowOutput_maxLatency' - The maximum latency in milliseconds. This parameter applies only to
-- RIST-based, Zixi-based, and Fujitsu-based streams.
--
-- 'mediaStreamOutputConfigurations', 'updateFlowOutput_mediaStreamOutputConfigurations' - The media streams that are associated with the output, and the
-- parameters for those associations.
--
-- 'smoothingLatency', 'updateFlowOutput_smoothingLatency' - The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
--
-- 'cidrAllowList', 'updateFlowOutput_cidrAllowList' - The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'streamId', 'updateFlowOutput_streamId' - The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
--
-- 'remoteId', 'updateFlowOutput_remoteId' - The remote ID for the Zixi-pull stream.
--
-- 'senderIpAddress', 'updateFlowOutput_senderIpAddress' - The IP address that the flow communicates with to initiate connection
-- with the sender.
--
-- 'vpcInterfaceAttachment', 'updateFlowOutput_vpcInterfaceAttachment' - The name of the VPC interface attachment to use for this output.
--
-- 'description', 'updateFlowOutput_description' - A description of the output. This description appears only on the AWS
-- Elemental MediaConnect console and will not be seen by the end user.
--
-- 'senderControlPort', 'updateFlowOutput_senderControlPort' - The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
--
-- 'encryption', 'updateFlowOutput_encryption' - The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
--
-- 'protocol', 'updateFlowOutput_protocol' - The protocol to use for the output.
--
-- 'minLatency', 'updateFlowOutput_minLatency' - The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
--
-- 'flowArn', 'updateFlowOutput_flowArn' - The flow that is associated with the output that you want to update.
--
-- 'outputArn', 'updateFlowOutput_outputArn' - The ARN of the output that you want to update.
newUpdateFlowOutput ::
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'outputArn'
  Prelude.Text ->
  UpdateFlowOutput
newUpdateFlowOutput pFlowArn_ pOutputArn_ =
  UpdateFlowOutput'
    { destination = Prelude.Nothing,
      port = Prelude.Nothing,
      maxLatency = Prelude.Nothing,
      mediaStreamOutputConfigurations = Prelude.Nothing,
      smoothingLatency = Prelude.Nothing,
      cidrAllowList = Prelude.Nothing,
      streamId = Prelude.Nothing,
      remoteId = Prelude.Nothing,
      senderIpAddress = Prelude.Nothing,
      vpcInterfaceAttachment = Prelude.Nothing,
      description = Prelude.Nothing,
      senderControlPort = Prelude.Nothing,
      encryption = Prelude.Nothing,
      protocol = Prelude.Nothing,
      minLatency = Prelude.Nothing,
      flowArn = pFlowArn_,
      outputArn = pOutputArn_
    }

-- | The IP address where you want to send the output.
updateFlowOutput_destination :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Text)
updateFlowOutput_destination = Lens.lens (\UpdateFlowOutput' {destination} -> destination) (\s@UpdateFlowOutput' {} a -> s {destination = a} :: UpdateFlowOutput)

-- | The port to use when content is distributed to this output.
updateFlowOutput_port :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Int)
updateFlowOutput_port = Lens.lens (\UpdateFlowOutput' {port} -> port) (\s@UpdateFlowOutput' {} a -> s {port = a} :: UpdateFlowOutput)

-- | The maximum latency in milliseconds. This parameter applies only to
-- RIST-based, Zixi-based, and Fujitsu-based streams.
updateFlowOutput_maxLatency :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Int)
updateFlowOutput_maxLatency = Lens.lens (\UpdateFlowOutput' {maxLatency} -> maxLatency) (\s@UpdateFlowOutput' {} a -> s {maxLatency = a} :: UpdateFlowOutput)

-- | The media streams that are associated with the output, and the
-- parameters for those associations.
updateFlowOutput_mediaStreamOutputConfigurations :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe [MediaStreamOutputConfigurationRequest])
updateFlowOutput_mediaStreamOutputConfigurations = Lens.lens (\UpdateFlowOutput' {mediaStreamOutputConfigurations} -> mediaStreamOutputConfigurations) (\s@UpdateFlowOutput' {} a -> s {mediaStreamOutputConfigurations = a} :: UpdateFlowOutput) Prelude.. Lens.mapping Lens.coerced

-- | The smoothing latency in milliseconds for RIST, RTP, and RTP-FEC
-- streams.
updateFlowOutput_smoothingLatency :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Int)
updateFlowOutput_smoothingLatency = Lens.lens (\UpdateFlowOutput' {smoothingLatency} -> smoothingLatency) (\s@UpdateFlowOutput' {} a -> s {smoothingLatency = a} :: UpdateFlowOutput)

-- | The range of IP addresses that should be allowed to initiate output
-- requests to this flow. These IP addresses should be in the form of a
-- Classless Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
updateFlowOutput_cidrAllowList :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe [Prelude.Text])
updateFlowOutput_cidrAllowList = Lens.lens (\UpdateFlowOutput' {cidrAllowList} -> cidrAllowList) (\s@UpdateFlowOutput' {} a -> s {cidrAllowList = a} :: UpdateFlowOutput) Prelude.. Lens.mapping Lens.coerced

-- | The stream ID that you want to use for this transport. This parameter
-- applies only to Zixi-based streams.
updateFlowOutput_streamId :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Text)
updateFlowOutput_streamId = Lens.lens (\UpdateFlowOutput' {streamId} -> streamId) (\s@UpdateFlowOutput' {} a -> s {streamId = a} :: UpdateFlowOutput)

-- | The remote ID for the Zixi-pull stream.
updateFlowOutput_remoteId :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Text)
updateFlowOutput_remoteId = Lens.lens (\UpdateFlowOutput' {remoteId} -> remoteId) (\s@UpdateFlowOutput' {} a -> s {remoteId = a} :: UpdateFlowOutput)

-- | The IP address that the flow communicates with to initiate connection
-- with the sender.
updateFlowOutput_senderIpAddress :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Text)
updateFlowOutput_senderIpAddress = Lens.lens (\UpdateFlowOutput' {senderIpAddress} -> senderIpAddress) (\s@UpdateFlowOutput' {} a -> s {senderIpAddress = a} :: UpdateFlowOutput)

-- | The name of the VPC interface attachment to use for this output.
updateFlowOutput_vpcInterfaceAttachment :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe VpcInterfaceAttachment)
updateFlowOutput_vpcInterfaceAttachment = Lens.lens (\UpdateFlowOutput' {vpcInterfaceAttachment} -> vpcInterfaceAttachment) (\s@UpdateFlowOutput' {} a -> s {vpcInterfaceAttachment = a} :: UpdateFlowOutput)

-- | A description of the output. This description appears only on the AWS
-- Elemental MediaConnect console and will not be seen by the end user.
updateFlowOutput_description :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Text)
updateFlowOutput_description = Lens.lens (\UpdateFlowOutput' {description} -> description) (\s@UpdateFlowOutput' {} a -> s {description = a} :: UpdateFlowOutput)

-- | The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
updateFlowOutput_senderControlPort :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Int)
updateFlowOutput_senderControlPort = Lens.lens (\UpdateFlowOutput' {senderControlPort} -> senderControlPort) (\s@UpdateFlowOutput' {} a -> s {senderControlPort = a} :: UpdateFlowOutput)

-- | The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
updateFlowOutput_encryption :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe UpdateEncryption)
updateFlowOutput_encryption = Lens.lens (\UpdateFlowOutput' {encryption} -> encryption) (\s@UpdateFlowOutput' {} a -> s {encryption = a} :: UpdateFlowOutput)

-- | The protocol to use for the output.
updateFlowOutput_protocol :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Protocol)
updateFlowOutput_protocol = Lens.lens (\UpdateFlowOutput' {protocol} -> protocol) (\s@UpdateFlowOutput' {} a -> s {protocol = a} :: UpdateFlowOutput)

-- | The minimum latency in milliseconds for SRT-based streams. In streams
-- that use the SRT protocol, this value that you set on your MediaConnect
-- source or output represents the minimal potential latency of that
-- connection. The latency of the stream is set to the highest number
-- between the sender’s minimum latency and the receiver’s minimum latency.
updateFlowOutput_minLatency :: Lens.Lens' UpdateFlowOutput (Prelude.Maybe Prelude.Int)
updateFlowOutput_minLatency = Lens.lens (\UpdateFlowOutput' {minLatency} -> minLatency) (\s@UpdateFlowOutput' {} a -> s {minLatency = a} :: UpdateFlowOutput)

-- | The flow that is associated with the output that you want to update.
updateFlowOutput_flowArn :: Lens.Lens' UpdateFlowOutput Prelude.Text
updateFlowOutput_flowArn = Lens.lens (\UpdateFlowOutput' {flowArn} -> flowArn) (\s@UpdateFlowOutput' {} a -> s {flowArn = a} :: UpdateFlowOutput)

-- | The ARN of the output that you want to update.
updateFlowOutput_outputArn :: Lens.Lens' UpdateFlowOutput Prelude.Text
updateFlowOutput_outputArn = Lens.lens (\UpdateFlowOutput' {outputArn} -> outputArn) (\s@UpdateFlowOutput' {} a -> s {outputArn = a} :: UpdateFlowOutput)

instance Core.AWSRequest UpdateFlowOutput where
  type
    AWSResponse UpdateFlowOutput =
      UpdateFlowOutputResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFlowOutputResponse'
            Prelude.<$> (x Core..?> "output")
            Prelude.<*> (x Core..?> "flowArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFlowOutput where
  hashWithSalt _salt UpdateFlowOutput' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` maxLatency
      `Prelude.hashWithSalt` mediaStreamOutputConfigurations
      `Prelude.hashWithSalt` smoothingLatency
      `Prelude.hashWithSalt` cidrAllowList
      `Prelude.hashWithSalt` streamId
      `Prelude.hashWithSalt` remoteId
      `Prelude.hashWithSalt` senderIpAddress
      `Prelude.hashWithSalt` vpcInterfaceAttachment
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` senderControlPort
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` minLatency
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` outputArn

instance Prelude.NFData UpdateFlowOutput where
  rnf UpdateFlowOutput' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf maxLatency
      `Prelude.seq` Prelude.rnf mediaStreamOutputConfigurations
      `Prelude.seq` Prelude.rnf smoothingLatency
      `Prelude.seq` Prelude.rnf cidrAllowList
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf remoteId
      `Prelude.seq` Prelude.rnf senderIpAddress
      `Prelude.seq` Prelude.rnf vpcInterfaceAttachment
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf senderControlPort
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf minLatency
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf outputArn

instance Core.ToHeaders UpdateFlowOutput where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateFlowOutput where
  toJSON UpdateFlowOutput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("destination" Core..=) Prelude.<$> destination,
            ("port" Core..=) Prelude.<$> port,
            ("maxLatency" Core..=) Prelude.<$> maxLatency,
            ("mediaStreamOutputConfigurations" Core..=)
              Prelude.<$> mediaStreamOutputConfigurations,
            ("smoothingLatency" Core..=)
              Prelude.<$> smoothingLatency,
            ("cidrAllowList" Core..=) Prelude.<$> cidrAllowList,
            ("streamId" Core..=) Prelude.<$> streamId,
            ("remoteId" Core..=) Prelude.<$> remoteId,
            ("senderIpAddress" Core..=)
              Prelude.<$> senderIpAddress,
            ("vpcInterfaceAttachment" Core..=)
              Prelude.<$> vpcInterfaceAttachment,
            ("description" Core..=) Prelude.<$> description,
            ("senderControlPort" Core..=)
              Prelude.<$> senderControlPort,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("protocol" Core..=) Prelude.<$> protocol,
            ("minLatency" Core..=) Prelude.<$> minLatency
          ]
      )

instance Core.ToPath UpdateFlowOutput where
  toPath UpdateFlowOutput' {..} =
    Prelude.mconcat
      [ "/v1/flows/",
        Core.toBS flowArn,
        "/outputs/",
        Core.toBS outputArn
      ]

instance Core.ToQuery UpdateFlowOutput where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFlowOutputResponse' smart constructor.
data UpdateFlowOutputResponse = UpdateFlowOutputResponse'
  { -- | The new settings of the output that you updated.
    output :: Prelude.Maybe Output,
    -- | The ARN of the flow that is associated with the updated output.
    flowArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFlowOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'updateFlowOutputResponse_output' - The new settings of the output that you updated.
--
-- 'flowArn', 'updateFlowOutputResponse_flowArn' - The ARN of the flow that is associated with the updated output.
--
-- 'httpStatus', 'updateFlowOutputResponse_httpStatus' - The response's http status code.
newUpdateFlowOutputResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFlowOutputResponse
newUpdateFlowOutputResponse pHttpStatus_ =
  UpdateFlowOutputResponse'
    { output = Prelude.Nothing,
      flowArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new settings of the output that you updated.
updateFlowOutputResponse_output :: Lens.Lens' UpdateFlowOutputResponse (Prelude.Maybe Output)
updateFlowOutputResponse_output = Lens.lens (\UpdateFlowOutputResponse' {output} -> output) (\s@UpdateFlowOutputResponse' {} a -> s {output = a} :: UpdateFlowOutputResponse)

-- | The ARN of the flow that is associated with the updated output.
updateFlowOutputResponse_flowArn :: Lens.Lens' UpdateFlowOutputResponse (Prelude.Maybe Prelude.Text)
updateFlowOutputResponse_flowArn = Lens.lens (\UpdateFlowOutputResponse' {flowArn} -> flowArn) (\s@UpdateFlowOutputResponse' {} a -> s {flowArn = a} :: UpdateFlowOutputResponse)

-- | The response's http status code.
updateFlowOutputResponse_httpStatus :: Lens.Lens' UpdateFlowOutputResponse Prelude.Int
updateFlowOutputResponse_httpStatus = Lens.lens (\UpdateFlowOutputResponse' {httpStatus} -> httpStatus) (\s@UpdateFlowOutputResponse' {} a -> s {httpStatus = a} :: UpdateFlowOutputResponse)

instance Prelude.NFData UpdateFlowOutputResponse where
  rnf UpdateFlowOutputResponse' {..} =
    Prelude.rnf output
      `Prelude.seq` Prelude.rnf flowArn
      `Prelude.seq` Prelude.rnf httpStatus
