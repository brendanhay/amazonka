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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.MediaStreamOutputConfiguration
import Amazonka.MediaConnect.Types.Transport
import Amazonka.MediaConnect.Types.VpcInterfaceAttachment
import qualified Amazonka.Prelude as Prelude

-- | The settings for an output.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | A description of the output.
    description :: Prelude.Maybe Prelude.Text,
    -- | The address where you want to send the output.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The type of key used for the encryption. If no keyType is provided, the
    -- service will use the default setting (static-key).
    encryption :: Prelude.Maybe Encryption,
    -- | The ARN of the entitlement on the originator\'\'s flow. This value is
    -- relevant only on entitled flows.
    entitlementArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address that the receiver requires in order to establish a
    -- connection with the flow. For public networking, the ListenerAddress is
    -- represented by the elastic IP address of the flow. For private
    -- networking, the ListenerAddress is represented by the elastic network
    -- interface IP address of the VPC. This field applies only to outputs that
    -- use the Zixi pull or SRT listener protocol.
    listenerAddress :: Prelude.Maybe Prelude.Text,
    -- | The input ARN of the AWS Elemental MediaLive channel. This parameter is
    -- relevant only for outputs that were added by creating a MediaLive input.
    mediaLiveInputArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration for each media stream that is associated with the
    -- output.
    mediaStreamOutputConfigurations :: Prelude.Maybe [MediaStreamOutputConfiguration],
    -- | The port to use when content is distributed to this output.
    port :: Prelude.Maybe Prelude.Int,
    -- | Attributes related to the transport stream that are used in the output.
    transport :: Prelude.Maybe Transport,
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
-- 'dataTransferSubscriberFeePercent', 'output_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'description', 'output_description' - A description of the output.
--
-- 'destination', 'output_destination' - The address where you want to send the output.
--
-- 'encryption', 'output_encryption' - The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
--
-- 'entitlementArn', 'output_entitlementArn' - The ARN of the entitlement on the originator\'\'s flow. This value is
-- relevant only on entitled flows.
--
-- 'listenerAddress', 'output_listenerAddress' - The IP address that the receiver requires in order to establish a
-- connection with the flow. For public networking, the ListenerAddress is
-- represented by the elastic IP address of the flow. For private
-- networking, the ListenerAddress is represented by the elastic network
-- interface IP address of the VPC. This field applies only to outputs that
-- use the Zixi pull or SRT listener protocol.
--
-- 'mediaLiveInputArn', 'output_mediaLiveInputArn' - The input ARN of the AWS Elemental MediaLive channel. This parameter is
-- relevant only for outputs that were added by creating a MediaLive input.
--
-- 'mediaStreamOutputConfigurations', 'output_mediaStreamOutputConfigurations' - The configuration for each media stream that is associated with the
-- output.
--
-- 'port', 'output_port' - The port to use when content is distributed to this output.
--
-- 'transport', 'output_transport' - Attributes related to the transport stream that are used in the output.
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
    { dataTransferSubscriberFeePercent =
        Prelude.Nothing,
      description = Prelude.Nothing,
      destination = Prelude.Nothing,
      encryption = Prelude.Nothing,
      entitlementArn = Prelude.Nothing,
      listenerAddress = Prelude.Nothing,
      mediaLiveInputArn = Prelude.Nothing,
      mediaStreamOutputConfigurations = Prelude.Nothing,
      port = Prelude.Nothing,
      transport = Prelude.Nothing,
      vpcInterfaceAttachment = Prelude.Nothing,
      outputArn = pOutputArn_,
      name = pName_
    }

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
output_dataTransferSubscriberFeePercent :: Lens.Lens' Output (Prelude.Maybe Prelude.Int)
output_dataTransferSubscriberFeePercent = Lens.lens (\Output' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@Output' {} a -> s {dataTransferSubscriberFeePercent = a} :: Output)

-- | A description of the output.
output_description :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_description = Lens.lens (\Output' {description} -> description) (\s@Output' {} a -> s {description = a} :: Output)

-- | The address where you want to send the output.
output_destination :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_destination = Lens.lens (\Output' {destination} -> destination) (\s@Output' {} a -> s {destination = a} :: Output)

-- | The type of key used for the encryption. If no keyType is provided, the
-- service will use the default setting (static-key).
output_encryption :: Lens.Lens' Output (Prelude.Maybe Encryption)
output_encryption = Lens.lens (\Output' {encryption} -> encryption) (\s@Output' {} a -> s {encryption = a} :: Output)

-- | The ARN of the entitlement on the originator\'\'s flow. This value is
-- relevant only on entitled flows.
output_entitlementArn :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_entitlementArn = Lens.lens (\Output' {entitlementArn} -> entitlementArn) (\s@Output' {} a -> s {entitlementArn = a} :: Output)

-- | The IP address that the receiver requires in order to establish a
-- connection with the flow. For public networking, the ListenerAddress is
-- represented by the elastic IP address of the flow. For private
-- networking, the ListenerAddress is represented by the elastic network
-- interface IP address of the VPC. This field applies only to outputs that
-- use the Zixi pull or SRT listener protocol.
output_listenerAddress :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_listenerAddress = Lens.lens (\Output' {listenerAddress} -> listenerAddress) (\s@Output' {} a -> s {listenerAddress = a} :: Output)

-- | The input ARN of the AWS Elemental MediaLive channel. This parameter is
-- relevant only for outputs that were added by creating a MediaLive input.
output_mediaLiveInputArn :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_mediaLiveInputArn = Lens.lens (\Output' {mediaLiveInputArn} -> mediaLiveInputArn) (\s@Output' {} a -> s {mediaLiveInputArn = a} :: Output)

-- | The configuration for each media stream that is associated with the
-- output.
output_mediaStreamOutputConfigurations :: Lens.Lens' Output (Prelude.Maybe [MediaStreamOutputConfiguration])
output_mediaStreamOutputConfigurations = Lens.lens (\Output' {mediaStreamOutputConfigurations} -> mediaStreamOutputConfigurations) (\s@Output' {} a -> s {mediaStreamOutputConfigurations = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | The port to use when content is distributed to this output.
output_port :: Lens.Lens' Output (Prelude.Maybe Prelude.Int)
output_port = Lens.lens (\Output' {port} -> port) (\s@Output' {} a -> s {port = a} :: Output)

-- | Attributes related to the transport stream that are used in the output.
output_transport :: Lens.Lens' Output (Prelude.Maybe Transport)
output_transport = Lens.lens (\Output' {transport} -> transport) (\s@Output' {} a -> s {transport = a} :: Output)

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

instance Data.FromJSON Output where
  parseJSON =
    Data.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> (x Data..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "entitlementArn")
            Prelude.<*> (x Data..:? "listenerAddress")
            Prelude.<*> (x Data..:? "mediaLiveInputArn")
            Prelude.<*> ( x
                            Data..:? "mediaStreamOutputConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "transport")
            Prelude.<*> (x Data..:? "vpcInterfaceAttachment")
            Prelude.<*> (x Data..: "outputArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Output where
  hashWithSalt _salt Output' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferSubscriberFeePercent
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` entitlementArn
      `Prelude.hashWithSalt` listenerAddress
      `Prelude.hashWithSalt` mediaLiveInputArn
      `Prelude.hashWithSalt` mediaStreamOutputConfigurations
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` transport
      `Prelude.hashWithSalt` vpcInterfaceAttachment
      `Prelude.hashWithSalt` outputArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf dataTransferSubscriberFeePercent `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf destination `Prelude.seq`
          Prelude.rnf encryption `Prelude.seq`
            Prelude.rnf entitlementArn `Prelude.seq`
              Prelude.rnf listenerAddress `Prelude.seq`
                Prelude.rnf mediaLiveInputArn `Prelude.seq`
                  Prelude.rnf mediaStreamOutputConfigurations `Prelude.seq`
                    Prelude.rnf port `Prelude.seq`
                      Prelude.rnf transport `Prelude.seq`
                        Prelude.rnf vpcInterfaceAttachment `Prelude.seq`
                          Prelude.rnf outputArn `Prelude.seq`
                            Prelude.rnf name
