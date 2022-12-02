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
-- Module      : Amazonka.WorkSpacesWeb.Types.NetworkSettingsSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.NetworkSettingsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of network settings.
--
-- /See:/ 'newNetworkSettingsSummary' smart constructor.
data NetworkSettingsSummary = NetworkSettingsSummary'
  { -- | The VPC ID of the network settings.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkSettingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'networkSettingsSummary_vpcId' - The VPC ID of the network settings.
--
-- 'networkSettingsArn', 'networkSettingsSummary_networkSettingsArn' - The ARN of the network settings.
newNetworkSettingsSummary ::
  NetworkSettingsSummary
newNetworkSettingsSummary =
  NetworkSettingsSummary'
    { vpcId = Prelude.Nothing,
      networkSettingsArn = Prelude.Nothing
    }

-- | The VPC ID of the network settings.
networkSettingsSummary_vpcId :: Lens.Lens' NetworkSettingsSummary (Prelude.Maybe Prelude.Text)
networkSettingsSummary_vpcId = Lens.lens (\NetworkSettingsSummary' {vpcId} -> vpcId) (\s@NetworkSettingsSummary' {} a -> s {vpcId = a} :: NetworkSettingsSummary)

-- | The ARN of the network settings.
networkSettingsSummary_networkSettingsArn :: Lens.Lens' NetworkSettingsSummary (Prelude.Maybe Prelude.Text)
networkSettingsSummary_networkSettingsArn = Lens.lens (\NetworkSettingsSummary' {networkSettingsArn} -> networkSettingsArn) (\s@NetworkSettingsSummary' {} a -> s {networkSettingsArn = a} :: NetworkSettingsSummary)

instance Data.FromJSON NetworkSettingsSummary where
  parseJSON =
    Data.withObject
      "NetworkSettingsSummary"
      ( \x ->
          NetworkSettingsSummary'
            Prelude.<$> (x Data..:? "vpcId")
            Prelude.<*> (x Data..:? "networkSettingsArn")
      )

instance Prelude.Hashable NetworkSettingsSummary where
  hashWithSalt _salt NetworkSettingsSummary' {..} =
    _salt `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` networkSettingsArn

instance Prelude.NFData NetworkSettingsSummary where
  rnf NetworkSettingsSummary' {..} =
    Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf networkSettingsArn
