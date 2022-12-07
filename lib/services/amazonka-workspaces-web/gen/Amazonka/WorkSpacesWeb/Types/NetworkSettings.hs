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
-- Module      : Amazonka.WorkSpacesWeb.Types.NetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.NetworkSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A network settings resource that can be associated with a web portal.
-- Once associated with a web portal, network settings define how streaming
-- instances will connect with your specified VPC.
--
-- /See:/ 'newNetworkSettings' smart constructor.
data NetworkSettings = NetworkSettings'
  { -- | A list of web portal ARNs that this network settings is associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text],
    -- | One or more security groups used to control access from streaming
    -- instances to your VPC.
    securityGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The VPC that streaming instances will connect to.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The subnets in which network interfaces are created to connect streaming
    -- instances to your VPC. At least two of these subnets must be in
    -- different availability zones.
    subnetIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedPortalArns', 'networkSettings_associatedPortalArns' - A list of web portal ARNs that this network settings is associated with.
--
-- 'securityGroupIds', 'networkSettings_securityGroupIds' - One or more security groups used to control access from streaming
-- instances to your VPC.
--
-- 'vpcId', 'networkSettings_vpcId' - The VPC that streaming instances will connect to.
--
-- 'subnetIds', 'networkSettings_subnetIds' - The subnets in which network interfaces are created to connect streaming
-- instances to your VPC. At least two of these subnets must be in
-- different availability zones.
--
-- 'networkSettingsArn', 'networkSettings_networkSettingsArn' - The ARN of the network settings.
newNetworkSettings ::
  -- | 'networkSettingsArn'
  Prelude.Text ->
  NetworkSettings
newNetworkSettings pNetworkSettingsArn_ =
  NetworkSettings'
    { associatedPortalArns =
        Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      networkSettingsArn = pNetworkSettingsArn_
    }

-- | A list of web portal ARNs that this network settings is associated with.
networkSettings_associatedPortalArns :: Lens.Lens' NetworkSettings (Prelude.Maybe [Prelude.Text])
networkSettings_associatedPortalArns = Lens.lens (\NetworkSettings' {associatedPortalArns} -> associatedPortalArns) (\s@NetworkSettings' {} a -> s {associatedPortalArns = a} :: NetworkSettings) Prelude.. Lens.mapping Lens.coerced

-- | One or more security groups used to control access from streaming
-- instances to your VPC.
networkSettings_securityGroupIds :: Lens.Lens' NetworkSettings (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkSettings_securityGroupIds = Lens.lens (\NetworkSettings' {securityGroupIds} -> securityGroupIds) (\s@NetworkSettings' {} a -> s {securityGroupIds = a} :: NetworkSettings) Prelude.. Lens.mapping Lens.coerced

-- | The VPC that streaming instances will connect to.
networkSettings_vpcId :: Lens.Lens' NetworkSettings (Prelude.Maybe Prelude.Text)
networkSettings_vpcId = Lens.lens (\NetworkSettings' {vpcId} -> vpcId) (\s@NetworkSettings' {} a -> s {vpcId = a} :: NetworkSettings)

-- | The subnets in which network interfaces are created to connect streaming
-- instances to your VPC. At least two of these subnets must be in
-- different availability zones.
networkSettings_subnetIds :: Lens.Lens' NetworkSettings (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
networkSettings_subnetIds = Lens.lens (\NetworkSettings' {subnetIds} -> subnetIds) (\s@NetworkSettings' {} a -> s {subnetIds = a} :: NetworkSettings) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the network settings.
networkSettings_networkSettingsArn :: Lens.Lens' NetworkSettings Prelude.Text
networkSettings_networkSettingsArn = Lens.lens (\NetworkSettings' {networkSettingsArn} -> networkSettingsArn) (\s@NetworkSettings' {} a -> s {networkSettingsArn = a} :: NetworkSettings)

instance Data.FromJSON NetworkSettings where
  parseJSON =
    Data.withObject
      "NetworkSettings"
      ( \x ->
          NetworkSettings'
            Prelude.<$> ( x Data..:? "associatedPortalArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "securityGroupIds")
            Prelude.<*> (x Data..:? "vpcId")
            Prelude.<*> (x Data..:? "subnetIds")
            Prelude.<*> (x Data..: "networkSettingsArn")
      )

instance Prelude.Hashable NetworkSettings where
  hashWithSalt _salt NetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` associatedPortalArns
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` networkSettingsArn

instance Prelude.NFData NetworkSettings where
  rnf NetworkSettings' {..} =
    Prelude.rnf associatedPortalArns
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf networkSettingsArn
