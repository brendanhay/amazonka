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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConnect.Types.Encryption
import Amazonka.MediaConnect.Types.MediaStreamSourceConfiguration
import Amazonka.MediaConnect.Types.Transport
import qualified Amazonka.Prelude as Prelude

-- | The settings for the source of the flow.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | The ARN of the entitlement that allows you to subscribe to content that
    -- comes from another AWS account. The entitlement is set by the content
    -- originator and the ARN is generated as part of the originator\'s flow.
    entitlementArn :: Prelude.Maybe Prelude.Text,
    -- | Percentage from 0-100 of the data transfer cost to be billed to the
    -- subscriber.
    dataTransferSubscriberFeePercent :: Prelude.Maybe Prelude.Int,
    -- | The name of the VPC interface that is used for this source.
    vpcInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The type of encryption that is used on the content ingested from this
    -- source.
    decryption :: Prelude.Maybe Encryption,
    -- | The IP address that the flow will be listening on for incoming content.
    ingestIp :: Prelude.Maybe Prelude.Text,
    -- | The port that the flow will be listening on for incoming content.
    ingestPort :: Prelude.Maybe Prelude.Int,
    -- | Attributes related to the transport stream that are used in the source.
    transport :: Prelude.Maybe Transport,
    -- | A description for the source. This value is not used or seen outside of
    -- the current AWS Elemental MediaConnect account.
    description :: Prelude.Maybe Prelude.Text,
    -- | The media streams that are associated with the source, and the
    -- parameters for those associations.
    mediaStreamSourceConfigurations :: Prelude.Maybe [MediaStreamSourceConfiguration],
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
-- 'entitlementArn', 'source_entitlementArn' - The ARN of the entitlement that allows you to subscribe to content that
-- comes from another AWS account. The entitlement is set by the content
-- originator and the ARN is generated as part of the originator\'s flow.
--
-- 'dataTransferSubscriberFeePercent', 'source_dataTransferSubscriberFeePercent' - Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
--
-- 'vpcInterfaceName', 'source_vpcInterfaceName' - The name of the VPC interface that is used for this source.
--
-- 'decryption', 'source_decryption' - The type of encryption that is used on the content ingested from this
-- source.
--
-- 'ingestIp', 'source_ingestIp' - The IP address that the flow will be listening on for incoming content.
--
-- 'ingestPort', 'source_ingestPort' - The port that the flow will be listening on for incoming content.
--
-- 'transport', 'source_transport' - Attributes related to the transport stream that are used in the source.
--
-- 'description', 'source_description' - A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
--
-- 'mediaStreamSourceConfigurations', 'source_mediaStreamSourceConfigurations' - The media streams that are associated with the source, and the
-- parameters for those associations.
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
    { entitlementArn = Prelude.Nothing,
      dataTransferSubscriberFeePercent = Prelude.Nothing,
      vpcInterfaceName = Prelude.Nothing,
      decryption = Prelude.Nothing,
      ingestIp = Prelude.Nothing,
      ingestPort = Prelude.Nothing,
      transport = Prelude.Nothing,
      description = Prelude.Nothing,
      mediaStreamSourceConfigurations = Prelude.Nothing,
      whitelistCidr = Prelude.Nothing,
      sourceArn = pSourceArn_,
      name = pName_
    }

-- | The ARN of the entitlement that allows you to subscribe to content that
-- comes from another AWS account. The entitlement is set by the content
-- originator and the ARN is generated as part of the originator\'s flow.
source_entitlementArn :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_entitlementArn = Lens.lens (\Source' {entitlementArn} -> entitlementArn) (\s@Source' {} a -> s {entitlementArn = a} :: Source)

-- | Percentage from 0-100 of the data transfer cost to be billed to the
-- subscriber.
source_dataTransferSubscriberFeePercent :: Lens.Lens' Source (Prelude.Maybe Prelude.Int)
source_dataTransferSubscriberFeePercent = Lens.lens (\Source' {dataTransferSubscriberFeePercent} -> dataTransferSubscriberFeePercent) (\s@Source' {} a -> s {dataTransferSubscriberFeePercent = a} :: Source)

-- | The name of the VPC interface that is used for this source.
source_vpcInterfaceName :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_vpcInterfaceName = Lens.lens (\Source' {vpcInterfaceName} -> vpcInterfaceName) (\s@Source' {} a -> s {vpcInterfaceName = a} :: Source)

-- | The type of encryption that is used on the content ingested from this
-- source.
source_decryption :: Lens.Lens' Source (Prelude.Maybe Encryption)
source_decryption = Lens.lens (\Source' {decryption} -> decryption) (\s@Source' {} a -> s {decryption = a} :: Source)

-- | The IP address that the flow will be listening on for incoming content.
source_ingestIp :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_ingestIp = Lens.lens (\Source' {ingestIp} -> ingestIp) (\s@Source' {} a -> s {ingestIp = a} :: Source)

-- | The port that the flow will be listening on for incoming content.
source_ingestPort :: Lens.Lens' Source (Prelude.Maybe Prelude.Int)
source_ingestPort = Lens.lens (\Source' {ingestPort} -> ingestPort) (\s@Source' {} a -> s {ingestPort = a} :: Source)

-- | Attributes related to the transport stream that are used in the source.
source_transport :: Lens.Lens' Source (Prelude.Maybe Transport)
source_transport = Lens.lens (\Source' {transport} -> transport) (\s@Source' {} a -> s {transport = a} :: Source)

-- | A description for the source. This value is not used or seen outside of
-- the current AWS Elemental MediaConnect account.
source_description :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_description = Lens.lens (\Source' {description} -> description) (\s@Source' {} a -> s {description = a} :: Source)

-- | The media streams that are associated with the source, and the
-- parameters for those associations.
source_mediaStreamSourceConfigurations :: Lens.Lens' Source (Prelude.Maybe [MediaStreamSourceConfiguration])
source_mediaStreamSourceConfigurations = Lens.lens (\Source' {mediaStreamSourceConfigurations} -> mediaStreamSourceConfigurations) (\s@Source' {} a -> s {mediaStreamSourceConfigurations = a} :: Source) Prelude.. Lens.mapping Lens.coerced

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

instance Core.FromJSON Source where
  parseJSON =
    Core.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Core..:? "entitlementArn")
            Prelude.<*> (x Core..:? "dataTransferSubscriberFeePercent")
            Prelude.<*> (x Core..:? "vpcInterfaceName")
            Prelude.<*> (x Core..:? "decryption")
            Prelude.<*> (x Core..:? "ingestIp")
            Prelude.<*> (x Core..:? "ingestPort")
            Prelude.<*> (x Core..:? "transport")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> ( x Core..:? "mediaStreamSourceConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "whitelistCidr")
            Prelude.<*> (x Core..: "sourceArn")
            Prelude.<*> (x Core..: "name")
      )

instance Prelude.Hashable Source

instance Prelude.NFData Source
