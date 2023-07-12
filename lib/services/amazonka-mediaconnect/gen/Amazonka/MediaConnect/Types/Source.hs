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
-- Module      : Amazonka.MediaConnect.Types.Source
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.MediaStreamSourceConfiguration
import Amazonka.MediaConnect.Types.Transport
import qualified Amazonka.Prelude as Prelude

-- | The settings for the source of the flow.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | The type of encryption that is used on the content ingested from this
    -- source.
    decryption :: Prelude.Maybe Encryption,
    -- | A description for the source. This value is not used or seen outside of
    -- the current AWS Elemental MediaConnect account.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the entitlement that allows you to subscribe to content that
    -- comes from another AWS account. The entitlement is set by the content
    -- originator and the ARN is generated as part of the originator\'s flow.
    entitlementArn :: Prelude.Maybe Prelude.Text,
    -- | The IP address that the flow will be listening on for incoming content.
    ingestIp :: Prelude.Maybe Prelude.Text,
    -- | The port that the flow will be listening on for incoming content.
    ingestPort :: Prelude.Maybe Prelude.Int,
    -- | The media streams that are associated with the source, and the
    -- parameters for those associations.
    mediaStreamSourceConfigurations :: Prelude.Maybe [MediaStreamSourceConfiguration],
    -- | The port that the flow uses to send outbound requests to initiate
    -- connection with the sender.
    senderControlPort :: Prelude.Maybe Prelude.Int,
    -- | The IP address that the flow communicates with to initiate connection
    -- with the sender.
    senderIpAddress :: Prelude.Maybe Prelude.Text,
    -- | Attributes related to the transport stream that are used in the source.
    transport :: Prelude.Maybe Transport,
    -- | The name of the VPC interface that is used for this source.
    vpcInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The range of IP addresses that should be allowed to contribute content
    -- to your source. These IP addresses should be in the form of a Classless
    -- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
    whitelistCidr :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source.
    sourceArn :: Prelude.Text,
    -- | The name of the source.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataTransferSubscriberFeePercent', 'source_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'decryption', 'source_decryption' - The type of encryption that is used on the content ingested from this
-- source.
--
-- 'description', 'source_description' - A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
--
-- 'entitlementArn', 'source_entitlementArn' - The ARN of the entitlement that allows you to subscribe to content that
-- comes from another AWS account. The entitlement is set by the content
-- originator and the ARN is generated as part of the originator\'s flow.
--
-- 'ingestIp', 'source_ingestIp' - The IP address that the flow will be listening on for incoming content.
--
-- 'ingestPort', 'source_ingestPort' - The port that the flow will be listening on for incoming content.
--
-- 'mediaStreamSourceConfigurations', 'source_mediaStreamSourceConfigurations' - The media streams that are associated with the source, and the
-- parameters for those associations.
--
-- 'senderControlPort', 'source_senderControlPort' - The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
--
-- 'senderIpAddress', 'source_senderIpAddress' - The IP address that the flow communicates with to initiate connection
-- with the sender.
--
-- 'transport', 'source_transport' - Attributes related to the transport stream that are used in the source.
--
-- 'vpcInterfaceName', 'source_vpcInterfaceName' - The name of the VPC interface that is used for this source.
--
-- 'whitelistCidr', 'source_whitelistCidr' - The range of IP addresses that should be allowed to contribute content
-- to your source. These IP addresses should be in the form of a Classless
-- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
--
-- 'sourceArn', 'source_sourceArn' - The ARN of the source.
--
-- 'name', 'source_name' - The name of the source.
newSource ::
  -- | 'sourceArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  Source
newSource pSourceArn_ pName_ =
  Source'
    { dataTransferSubscriberFeePercent =
        Prelude.Nothing,
      decryption = Prelude.Nothing,
      description = Prelude.Nothing,
      entitlementArn = Prelude.Nothing,
      ingestIp = Prelude.Nothing,
      ingestPort = Prelude.Nothing,
      mediaStreamSourceConfigurations = Prelude.Nothing,
      senderControlPort = Prelude.Nothing,
      senderIpAddress = Prelude.Nothing,
      transport = Prelude.Nothing,
      vpcInterfaceName = Prelude.Nothing,
      whitelistCidr = Prelude.Nothing,
      sourceArn = pSourceArn_,
      name = pName_
    }

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
source_dataTransferSubscriberFeePercent :: Lens.Lens' Source (Prelude.Maybe Prelude.Int)
source_dataTransferSubscriberFeePercent = Lens.lens (\Source' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@Source' {} a -> s {dataTransferSubscriberFeePercent = a} :: Source)

-- | The type of encryption that is used on the content ingested from this
-- source.
source_decryption :: Lens.Lens' Source (Prelude.Maybe Encryption)
source_decryption = Lens.lens (\Source' {decryption} -> decryption) (\s@Source' {} a -> s {decryption = a} :: Source)

-- | A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
source_description :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_description = Lens.lens (\Source' {description} -> description) (\s@Source' {} a -> s {description = a} :: Source)

-- | The ARN of the entitlement that allows you to subscribe to content that
-- comes from another AWS account. The entitlement is set by the content
-- originator and the ARN is generated as part of the originator\'s flow.
source_entitlementArn :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_entitlementArn = Lens.lens (\Source' {entitlementArn} -> entitlementArn) (\s@Source' {} a -> s {entitlementArn = a} :: Source)

-- | The IP address that the flow will be listening on for incoming content.
source_ingestIp :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_ingestIp = Lens.lens (\Source' {ingestIp} -> ingestIp) (\s@Source' {} a -> s {ingestIp = a} :: Source)

-- | The port that the flow will be listening on for incoming content.
source_ingestPort :: Lens.Lens' Source (Prelude.Maybe Prelude.Int)
source_ingestPort = Lens.lens (\Source' {ingestPort} -> ingestPort) (\s@Source' {} a -> s {ingestPort = a} :: Source)

-- | The media streams that are associated with the source, and the
-- parameters for those associations.
source_mediaStreamSourceConfigurations :: Lens.Lens' Source (Prelude.Maybe [MediaStreamSourceConfiguration])
source_mediaStreamSourceConfigurations = Lens.lens (\Source' {mediaStreamSourceConfigurations} -> mediaStreamSourceConfigurations) (\s@Source' {} a -> s {mediaStreamSourceConfigurations = a} :: Source) Prelude.. Lens.mapping Lens.coerced

-- | The port that the flow uses to send outbound requests to initiate
-- connection with the sender.
source_senderControlPort :: Lens.Lens' Source (Prelude.Maybe Prelude.Int)
source_senderControlPort = Lens.lens (\Source' {senderControlPort} -> senderControlPort) (\s@Source' {} a -> s {senderControlPort = a} :: Source)

-- | The IP address that the flow communicates with to initiate connection
-- with the sender.
source_senderIpAddress :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_senderIpAddress = Lens.lens (\Source' {senderIpAddress} -> senderIpAddress) (\s@Source' {} a -> s {senderIpAddress = a} :: Source)

-- | Attributes related to the transport stream that are used in the source.
source_transport :: Lens.Lens' Source (Prelude.Maybe Transport)
source_transport = Lens.lens (\Source' {transport} -> transport) (\s@Source' {} a -> s {transport = a} :: Source)

-- | The name of the VPC interface that is used for this source.
source_vpcInterfaceName :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_vpcInterfaceName = Lens.lens (\Source' {vpcInterfaceName} -> vpcInterfaceName) (\s@Source' {} a -> s {vpcInterfaceName = a} :: Source)

-- | The range of IP addresses that should be allowed to contribute content
-- to your source. These IP addresses should be in the form of a Classless
-- Inter-Domain Routing (CIDR) block; for example, 10.0.0.0\/16.
source_whitelistCidr :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_whitelistCidr = Lens.lens (\Source' {whitelistCidr} -> whitelistCidr) (\s@Source' {} a -> s {whitelistCidr = a} :: Source)

-- | The ARN of the source.
source_sourceArn :: Lens.Lens' Source Prelude.Text
source_sourceArn = Lens.lens (\Source' {sourceArn} -> sourceArn) (\s@Source' {} a -> s {sourceArn = a} :: Source)

-- | The name of the source.
source_name :: Lens.Lens' Source Prelude.Text
source_name = Lens.lens (\Source' {name} -> name) (\s@Source' {} a -> s {name = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Data..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Data..:? "decryption")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "entitlementArn")
            Prelude.<*> (x Data..:? "ingestIp")
            Prelude.<*> (x Data..:? "ingestPort")
            Prelude.<*> ( x
                            Data..:? "mediaStreamSourceConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "senderControlPort")
            Prelude.<*> (x Data..:? "senderIpAddress")
            Prelude.<*> (x Data..:? "transport")
            Prelude.<*> (x Data..:? "vpcInterfaceName")
            Prelude.<*> (x Data..:? "whitelistCidr")
            Prelude.<*> (x Data..: "sourceArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt
      `Prelude.hashWithSalt` dataTransferSubscriberFeePercent
      `Prelude.hashWithSalt` decryption
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` entitlementArn
      `Prelude.hashWithSalt` ingestIp
      `Prelude.hashWithSalt` ingestPort
      `Prelude.hashWithSalt` mediaStreamSourceConfigurations
      `Prelude.hashWithSalt` senderControlPort
      `Prelude.hashWithSalt` senderIpAddress
      `Prelude.hashWithSalt` transport
      `Prelude.hashWithSalt` vpcInterfaceName
      `Prelude.hashWithSalt` whitelistCidr
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf dataTransferSubscriberFeePercent
      `Prelude.seq` Prelude.rnf decryption
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf entitlementArn
      `Prelude.seq` Prelude.rnf ingestIp
      `Prelude.seq` Prelude.rnf ingestPort
      `Prelude.seq` Prelude.rnf mediaStreamSourceConfigurations
      `Prelude.seq` Prelude.rnf senderControlPort
      `Prelude.seq` Prelude.rnf senderIpAddress
      `Prelude.seq` Prelude.rnf transport
      `Prelude.seq` Prelude.rnf vpcInterfaceName
      `Prelude.seq` Prelude.rnf whitelistCidr
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf name
