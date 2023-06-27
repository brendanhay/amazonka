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
-- Module      : Amazonka.MediaConnect.Types.Flow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.Flow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Entitlement
import Amazonka.MediaConnect.Types.FailoverConfig
import Amazonka.MediaConnect.Types.Maintenance
import Amazonka.MediaConnect.Types.MediaStream
import Amazonka.MediaConnect.Types.Output
import Amazonka.MediaConnect.Types.Source
import Amazonka.MediaConnect.Types.Status
import Amazonka.MediaConnect.Types.VpcInterface
import qualified Amazonka.Prelude as Prelude

-- | The settings for a flow, including its source, outputs, and
-- entitlements.
--
-- /See:/ 'newFlow' smart constructor.
data Flow = Flow'
  { -- | A description of the flow. This value is not used or seen outside of the
    -- current AWS Elemental MediaConnect account.
    description :: Prelude.Maybe Prelude.Text,
    -- | The IP address from which video will be sent to output destinations.
    egressIp :: Prelude.Maybe Prelude.Text,
    maintenance :: Prelude.Maybe Maintenance,
    -- | The media streams that are associated with the flow. After you associate
    -- a media stream with a source, you can also associate it with outputs on
    -- the flow.
    mediaStreams :: Prelude.Maybe [MediaStream],
    sourceFailoverConfig :: Prelude.Maybe FailoverConfig,
    sources :: Prelude.Maybe [Source],
    -- | The VPC Interfaces for this flow.
    vpcInterfaces :: Prelude.Maybe [VpcInterface],
    -- | The current status of the flow.
    status :: Status,
    -- | The Availability Zone that you want to create the flow in. These options
    -- are limited to the Availability Zones within the current AWS.
    availabilityZone :: Prelude.Text,
    source :: Source,
    -- | The name of the flow.
    name :: Prelude.Text,
    -- | The entitlements in this flow.
    entitlements :: [Entitlement],
    -- | The outputs in this flow.
    outputs :: [Output],
    -- | The Amazon Resource Name (ARN) of the flow.
    flowArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Flow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'flow_description' - A description of the flow. This value is not used or seen outside of the
-- current AWS Elemental MediaConnect account.
--
-- 'egressIp', 'flow_egressIp' - The IP address from which video will be sent to output destinations.
--
-- 'maintenance', 'flow_maintenance' - Undocumented member.
--
-- 'mediaStreams', 'flow_mediaStreams' - The media streams that are associated with the flow. After you associate
-- a media stream with a source, you can also associate it with outputs on
-- the flow.
--
-- 'sourceFailoverConfig', 'flow_sourceFailoverConfig' - Undocumented member.
--
-- 'sources', 'flow_sources' - Undocumented member.
--
-- 'vpcInterfaces', 'flow_vpcInterfaces' - The VPC Interfaces for this flow.
--
-- 'status', 'flow_status' - The current status of the flow.
--
-- 'availabilityZone', 'flow_availabilityZone' - The Availability Zone that you want to create the flow in. These options
-- are limited to the Availability Zones within the current AWS.
--
-- 'source', 'flow_source' - Undocumented member.
--
-- 'name', 'flow_name' - The name of the flow.
--
-- 'entitlements', 'flow_entitlements' - The entitlements in this flow.
--
-- 'outputs', 'flow_outputs' - The outputs in this flow.
--
-- 'flowArn', 'flow_flowArn' - The Amazon Resource Name (ARN) of the flow.
newFlow ::
  -- | 'status'
  Status ->
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'source'
  Source ->
  -- | 'name'
  Prelude.Text ->
  -- | 'flowArn'
  Prelude.Text ->
  Flow
newFlow
  pStatus_
  pAvailabilityZone_
  pSource_
  pName_
  pFlowArn_ =
    Flow'
      { description = Prelude.Nothing,
        egressIp = Prelude.Nothing,
        maintenance = Prelude.Nothing,
        mediaStreams = Prelude.Nothing,
        sourceFailoverConfig = Prelude.Nothing,
        sources = Prelude.Nothing,
        vpcInterfaces = Prelude.Nothing,
        status = pStatus_,
        availabilityZone = pAvailabilityZone_,
        source = pSource_,
        name = pName_,
        entitlements = Prelude.mempty,
        outputs = Prelude.mempty,
        flowArn = pFlowArn_
      }

-- | A description of the flow. This value is not used or seen outside of the
-- current AWS Elemental MediaConnect account.
flow_description :: Lens.Lens' Flow (Prelude.Maybe Prelude.Text)
flow_description = Lens.lens (\Flow' {description} -> description) (\s@Flow' {} a -> s {description = a} :: Flow)

-- | The IP address from which video will be sent to output destinations.
flow_egressIp :: Lens.Lens' Flow (Prelude.Maybe Prelude.Text)
flow_egressIp = Lens.lens (\Flow' {egressIp} -> egressIp) (\s@Flow' {} a -> s {egressIp = a} :: Flow)

-- | Undocumented member.
flow_maintenance :: Lens.Lens' Flow (Prelude.Maybe Maintenance)
flow_maintenance = Lens.lens (\Flow' {maintenance} -> maintenance) (\s@Flow' {} a -> s {maintenance = a} :: Flow)

-- | The media streams that are associated with the flow. After you associate
-- a media stream with a source, you can also associate it with outputs on
-- the flow.
flow_mediaStreams :: Lens.Lens' Flow (Prelude.Maybe [MediaStream])
flow_mediaStreams = Lens.lens (\Flow' {mediaStreams} -> mediaStreams) (\s@Flow' {} a -> s {mediaStreams = a} :: Flow) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
flow_sourceFailoverConfig :: Lens.Lens' Flow (Prelude.Maybe FailoverConfig)
flow_sourceFailoverConfig = Lens.lens (\Flow' {sourceFailoverConfig} -> sourceFailoverConfig) (\s@Flow' {} a -> s {sourceFailoverConfig = a} :: Flow)

-- | Undocumented member.
flow_sources :: Lens.Lens' Flow (Prelude.Maybe [Source])
flow_sources = Lens.lens (\Flow' {sources} -> sources) (\s@Flow' {} a -> s {sources = a} :: Flow) Prelude.. Lens.mapping Lens.coerced

-- | The VPC Interfaces for this flow.
flow_vpcInterfaces :: Lens.Lens' Flow (Prelude.Maybe [VpcInterface])
flow_vpcInterfaces = Lens.lens (\Flow' {vpcInterfaces} -> vpcInterfaces) (\s@Flow' {} a -> s {vpcInterfaces = a} :: Flow) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the flow.
flow_status :: Lens.Lens' Flow Status
flow_status = Lens.lens (\Flow' {status} -> status) (\s@Flow' {} a -> s {status = a} :: Flow)

-- | The Availability Zone that you want to create the flow in. These options
-- are limited to the Availability Zones within the current AWS.
flow_availabilityZone :: Lens.Lens' Flow Prelude.Text
flow_availabilityZone = Lens.lens (\Flow' {availabilityZone} -> availabilityZone) (\s@Flow' {} a -> s {availabilityZone = a} :: Flow)

-- | Undocumented member.
flow_source :: Lens.Lens' Flow Source
flow_source = Lens.lens (\Flow' {source} -> source) (\s@Flow' {} a -> s {source = a} :: Flow)

-- | The name of the flow.
flow_name :: Lens.Lens' Flow Prelude.Text
flow_name = Lens.lens (\Flow' {name} -> name) (\s@Flow' {} a -> s {name = a} :: Flow)

-- | The entitlements in this flow.
flow_entitlements :: Lens.Lens' Flow [Entitlement]
flow_entitlements = Lens.lens (\Flow' {entitlements} -> entitlements) (\s@Flow' {} a -> s {entitlements = a} :: Flow) Prelude.. Lens.coerced

-- | The outputs in this flow.
flow_outputs :: Lens.Lens' Flow [Output]
flow_outputs = Lens.lens (\Flow' {outputs} -> outputs) (\s@Flow' {} a -> s {outputs = a} :: Flow) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the flow.
flow_flowArn :: Lens.Lens' Flow Prelude.Text
flow_flowArn = Lens.lens (\Flow' {flowArn} -> flowArn) (\s@Flow' {} a -> s {flowArn = a} :: Flow)

instance Data.FromJSON Flow where
  parseJSON =
    Data.withObject
      "Flow"
      ( \x ->
          Flow'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "egressIp")
            Prelude.<*> (x Data..:? "maintenance")
            Prelude.<*> (x Data..:? "mediaStreams" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sourceFailoverConfig")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcInterfaces" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "availabilityZone")
            Prelude.<*> (x Data..: "source")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..:? "entitlements" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "flowArn")
      )

instance Prelude.Hashable Flow where
  hashWithSalt _salt Flow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` egressIp
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` mediaStreams
      `Prelude.hashWithSalt` sourceFailoverConfig
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` vpcInterfaces
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` flowArn

instance Prelude.NFData Flow where
  rnf Flow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf egressIp
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf mediaStreams
      `Prelude.seq` Prelude.rnf sourceFailoverConfig
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf vpcInterfaces
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf flowArn
