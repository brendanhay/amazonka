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
-- Module      : Amazonka.MediaConnect.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.MediaStreamOutputConfiguration
import Amazonka.MediaConnect.Types.Transport
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The settings for an output.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The ARN of the entitlement on the originator\'\'s flow. This value is
    -- relevant only on entitled flows.
    entitlementArn :: Prelude.Maybe Prelude.Text,
    -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | The address where you want to send the output.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The configuration for each media stream that is associated with the
    -- output.
    mediaStreamOutputConfigurations :: Prelude.Maybe [MediaStreamOutputConfiguration],
    -- | The input ARN of the AWS Elemental MediaLive channel. This parameter is
    -- relevant only for outputs that were added by creating a MediaLive input.
    mediaLiveInputArn :: Prelude.Maybe Prelude.Text,
    -- | The type of key used for the encryption. If no keyType is provided, the
    -- service will use the default setting (static-key).
    encryption :: Prelude.Maybe Encryption,
    -- | The IP address that the receiver requires in order to establish a
    -- connection with the flow. For public networking, the ListenerAddress is
    -- represented by the elastic IP address of the flow. For private
    -- networking, the ListenerAddress is represented by the elastic network
    -- interface IP address of the VPC. This field applies only to outputs that
    -- use the Zixi pull or SRT listener protocol.
    listenerAddress :: Prelude.Maybe Prelude.Text,
    -- | Attributes related to the transport stream that are used in the output.
    transport :: Prelude.Maybe Transport,
    -- | A description of the output.
    description :: Prelude.Maybe Prelude.Text,
    -- | The port to use when content is distributed to this output.
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the VPC interface attachment to use for this output.
    vpcInterfaceAttachment :: Prelude.Maybe VpcInterfaceAttachment,
    -- | The ARN of the output.
    outputArn :: Prelude.Text,
    -- | The name of the output. This value must be unique within the current
    -- flow.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitlementArn', 'output_entitlementArn' - The ARN of the entitlement on the originator\'\'s flow. This value is
-- relevant only on entitled flows.
--
-- 'dataTransferSubscriberFeePercent', 'output_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'destination', 'output_destination' - The address where you want to send the output.
--
-- 'mediaStreamOutputConfigurations', 'output_mediaStreamOutputConfigurations' - The configuration for each media stream that is associated with the
-- output.
--
-- 'mediaLiveInputArn', 'output_mediaLiveInputArn' - The input ARN of the AWS Elemental MediaLive channel. This parameter is
-- relevant only for outputs that were added by creating a MediaLive input.
--
-- 'encryption', 'output_encryption' - The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
--
-- 'listenerAddress', 'output_listenerAddress' - The IP address that the receiver requires in order to establish a
-- connection with the flow. For public networking, the ListenerAddress is
-- represented by the elastic IP address of the flow. For private
-- networking, the ListenerAddress is represented by the elastic network
-- interface IP address of the VPC. This field applies only to outputs that
-- use the Zixi pull or SRT listener protocol.
--
-- 'transport', 'output_transport' - Attributes related to the transport stream that are used in the output.
--
-- 'description', 'output_description' - A description of the output.
--
-- 'port', 'output_port' - The port to use when content is distributed to this output.
--
-- 'vpcInterfaceAttachment', 'output_vpcInterfaceAttachment' - The name of the VPC interface attachment to use for this output.
--
-- 'outputArn', 'output_outputArn' - The ARN of the output.
--
-- 'name', 'output_name' - The name of the output. This value must be unique within the current
-- flow.
newOutput ::
  -- | 'outputArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Output
newOutput pOutputArn_ pName_ =
  Output'
    { entitlementArn = Prelude.Nothing,
      dataTransferSubscriberFeePercent = Prelude.Nothing,
      destination = Prelude.Nothing,
      mediaStreamOutputConfigurations = Prelude.Nothing,
      mediaLiveInputArn = Prelude.Nothing,
      encryption = Prelude.Nothing,
      listenerAddress = Prelude.Nothing,
      transport = Prelude.Nothing,
      description = Prelude.Nothing,
      port = Prelude.Nothing,
      vpcInterfaceAttachment = Prelude.Nothing,
      outputArn = pOutputArn_,
      name = pName_
    }

-- | The ARN of the entitlement on the originator\'\'s flow. This value is
-- relevant only on entitled flows.
output_entitlementArn :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_entitlementArn = Lens.lens (\Output' {entitlementArn} -> entitlementArn) (\s@Output' {} a -> s {entitlementArn = a} :: Output)

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
output_dataTransferSubscriberFeePercent :: Lens.Lens' Output (Prelude.Maybe Prelude.Int)
output_dataTransferSubscriberFeePercent = Lens.lens (\Output' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@Output' {} a -> s {dataTransferSubscriberFeePercent = a} :: Output)

-- | The address where you want to send the output.
output_destination :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_destination = Lens.lens (\Output' {destination} -> destination) (\s@Output' {} a -> s {destination = a} :: Output)

-- | The configuration for each media stream that is associated with the
-- output.
output_mediaStreamOutputConfigurations :: Lens.Lens' Output (Prelude.Maybe [MediaStreamOutputConfiguration])
output_mediaStreamOutputConfigurations = Lens.lens (\Output' {mediaStreamOutputConfigurations} -> mediaStreamOutputConfigurations) (\s@Output' {} a -> s {mediaStreamOutputConfigurations = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | The input ARN of the AWS Elemental MediaLive channel. This parameter is
-- relevant only for outputs that were added by creating a MediaLive input.
output_mediaLiveInputArn :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_mediaLiveInputArn = Lens.lens (\Output' {mediaLiveInputArn} -> mediaLiveInputArn) (\s@Output' {} a -> s {mediaLiveInputArn = a} :: Output)

-- | The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
output_encryption :: Lens.Lens' Output (Prelude.Maybe Encryption)
output_encryption = Lens.lens (\Output' {encryption} -> encryption) (\s@Output' {} a -> s {encryption = a} :: Output)

-- | The IP address that the receiver requires in order to establish a
-- connection with the flow. For public networking, the ListenerAddress is
-- represented by the elastic IP address of the flow. For private
-- networking, the ListenerAddress is represented by the elastic network
-- interface IP address of the VPC. This field applies only to outputs that
-- use the Zixi pull or SRT listener protocol.
output_listenerAddress :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_listenerAddress = Lens.lens (\Output' {listenerAddress} -> listenerAddress) (\s@Output' {} a -> s {listenerAddress = a} :: Output)

-- | Attributes related to the transport stream that are used in the output.
output_transport :: Lens.Lens' Output (Prelude.Maybe Transport)
output_transport = Lens.lens (\Output' {transport} -> transport) (\s@Output' {} a -> s {transport = a} :: Output)

-- | A description of the output.
output_description :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_description = Lens.lens (\Output' {description} -> description) (\s@Output' {} a -> s {description = a} :: Output)

-- | The port to use when content is distributed to this output.
output_port :: Lens.Lens' Output (Prelude.Maybe Prelude.Int)
output_port = Lens.lens (\Output' {port} -> port) (\s@Output' {} a -> s {port = a} :: Output)

-- | The name of the VPC interface attachment to use for this output.
output_vpcInterfaceAttachment :: Lens.Lens' Output (Prelude.Maybe VpcInterfaceAttachment)
output_vpcInterfaceAttachment = Lens.lens (\Output' {vpcInterfaceAttachment} -> vpcInterfaceAttachment) (\s@Output' {} a -> s {vpcInterfaceAttachment = a} :: Output)

-- | The ARN of the output.
output_outputArn :: Lens.Lens' Output Prelude.Text
output_outputArn = Lens.lens (\Output' {outputArn} -> outputArn) (\s@Output' {} a -> s {outputArn = a} :: Output)

-- | The name of the output. This value must be unique within the current
-- flow.
output_name :: Lens.Lens' Output Prelude.Text
output_name = Lens.lens (\Output' {name} -> name) (\s@Output' {} a -> s {name = a} :: Output)

instance Core.FromJSON Output where
  parseJSON =
    Core.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> (x Core..:? "entitlementArn")
            Prelude.<*> (x Core..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> ( x Core..:? "mediaStreamOutputConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "mediaLiveInputArn")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "listenerAddress")
            Prelude.<*> (x Core..:? "transport")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "port")
            Prelude.<*> (x Core..:? "vpcInterfaceAttachment")
            Prelude.<*> (x Core..: "outputArn")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Output

instance Prelude.NFData Output
