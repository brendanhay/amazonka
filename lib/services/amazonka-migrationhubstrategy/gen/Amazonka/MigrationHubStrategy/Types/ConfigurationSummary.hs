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
-- Module      : Amazonka.MigrationHubStrategy.Types.ConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.IPAddressBasedRemoteInfo
import Amazonka.MigrationHubStrategy.Types.PipelineInfo
import Amazonka.MigrationHubStrategy.Types.RemoteSourceCodeAnalysisServerInfo
import Amazonka.MigrationHubStrategy.Types.VcenterBasedRemoteInfo
import Amazonka.MigrationHubStrategy.Types.VersionControlInfo
import qualified Amazonka.Prelude as Prelude

-- | Summary of the collector configuration.
--
-- /See:/ 'newConfigurationSummary' smart constructor.
data ConfigurationSummary = ConfigurationSummary'
  { -- | IP address based configurations.
    ipAddressBasedRemoteInfoList :: Prelude.Maybe [IPAddressBasedRemoteInfo],
    -- | The list of pipeline info configurations.
    pipelineInfoList :: Prelude.Maybe [PipelineInfo],
    -- | Info about the remote server source code configuration.
    remoteSourceCodeAnalysisServerInfo :: Prelude.Maybe RemoteSourceCodeAnalysisServerInfo,
    -- | The list of vCenter configurations.
    vcenterBasedRemoteInfoList :: Prelude.Maybe [VcenterBasedRemoteInfo],
    -- | The list of the version control configurations.
    versionControlInfoList :: Prelude.Maybe [VersionControlInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressBasedRemoteInfoList', 'configurationSummary_ipAddressBasedRemoteInfoList' - IP address based configurations.
--
-- 'pipelineInfoList', 'configurationSummary_pipelineInfoList' - The list of pipeline info configurations.
--
-- 'remoteSourceCodeAnalysisServerInfo', 'configurationSummary_remoteSourceCodeAnalysisServerInfo' - Info about the remote server source code configuration.
--
-- 'vcenterBasedRemoteInfoList', 'configurationSummary_vcenterBasedRemoteInfoList' - The list of vCenter configurations.
--
-- 'versionControlInfoList', 'configurationSummary_versionControlInfoList' - The list of the version control configurations.
newConfigurationSummary ::
  ConfigurationSummary
newConfigurationSummary =
  ConfigurationSummary'
    { ipAddressBasedRemoteInfoList =
        Prelude.Nothing,
      pipelineInfoList = Prelude.Nothing,
      remoteSourceCodeAnalysisServerInfo = Prelude.Nothing,
      vcenterBasedRemoteInfoList = Prelude.Nothing,
      versionControlInfoList = Prelude.Nothing
    }

-- | IP address based configurations.
configurationSummary_ipAddressBasedRemoteInfoList :: Lens.Lens' ConfigurationSummary (Prelude.Maybe [IPAddressBasedRemoteInfo])
configurationSummary_ipAddressBasedRemoteInfoList = Lens.lens (\ConfigurationSummary' {ipAddressBasedRemoteInfoList} -> ipAddressBasedRemoteInfoList) (\s@ConfigurationSummary' {} a -> s {ipAddressBasedRemoteInfoList = a} :: ConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The list of pipeline info configurations.
configurationSummary_pipelineInfoList :: Lens.Lens' ConfigurationSummary (Prelude.Maybe [PipelineInfo])
configurationSummary_pipelineInfoList = Lens.lens (\ConfigurationSummary' {pipelineInfoList} -> pipelineInfoList) (\s@ConfigurationSummary' {} a -> s {pipelineInfoList = a} :: ConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | Info about the remote server source code configuration.
configurationSummary_remoteSourceCodeAnalysisServerInfo :: Lens.Lens' ConfigurationSummary (Prelude.Maybe RemoteSourceCodeAnalysisServerInfo)
configurationSummary_remoteSourceCodeAnalysisServerInfo = Lens.lens (\ConfigurationSummary' {remoteSourceCodeAnalysisServerInfo} -> remoteSourceCodeAnalysisServerInfo) (\s@ConfigurationSummary' {} a -> s {remoteSourceCodeAnalysisServerInfo = a} :: ConfigurationSummary)

-- | The list of vCenter configurations.
configurationSummary_vcenterBasedRemoteInfoList :: Lens.Lens' ConfigurationSummary (Prelude.Maybe [VcenterBasedRemoteInfo])
configurationSummary_vcenterBasedRemoteInfoList = Lens.lens (\ConfigurationSummary' {vcenterBasedRemoteInfoList} -> vcenterBasedRemoteInfoList) (\s@ConfigurationSummary' {} a -> s {vcenterBasedRemoteInfoList = a} :: ConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The list of the version control configurations.
configurationSummary_versionControlInfoList :: Lens.Lens' ConfigurationSummary (Prelude.Maybe [VersionControlInfo])
configurationSummary_versionControlInfoList = Lens.lens (\ConfigurationSummary' {versionControlInfoList} -> versionControlInfoList) (\s@ConfigurationSummary' {} a -> s {versionControlInfoList = a} :: ConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfigurationSummary where
  parseJSON =
    Data.withObject
      "ConfigurationSummary"
      ( \x ->
          ConfigurationSummary'
            Prelude.<$> ( x
                            Data..:? "ipAddressBasedRemoteInfoList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "pipelineInfoList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "remoteSourceCodeAnalysisServerInfo")
            Prelude.<*> ( x
                            Data..:? "vcenterBasedRemoteInfoList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "versionControlInfoList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConfigurationSummary where
  hashWithSalt _salt ConfigurationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddressBasedRemoteInfoList
      `Prelude.hashWithSalt` pipelineInfoList
      `Prelude.hashWithSalt` remoteSourceCodeAnalysisServerInfo
      `Prelude.hashWithSalt` vcenterBasedRemoteInfoList
      `Prelude.hashWithSalt` versionControlInfoList

instance Prelude.NFData ConfigurationSummary where
  rnf ConfigurationSummary' {..} =
    Prelude.rnf ipAddressBasedRemoteInfoList
      `Prelude.seq` Prelude.rnf pipelineInfoList
      `Prelude.seq` Prelude.rnf remoteSourceCodeAnalysisServerInfo
      `Prelude.seq` Prelude.rnf vcenterBasedRemoteInfoList
      `Prelude.seq` Prelude.rnf versionControlInfoList
