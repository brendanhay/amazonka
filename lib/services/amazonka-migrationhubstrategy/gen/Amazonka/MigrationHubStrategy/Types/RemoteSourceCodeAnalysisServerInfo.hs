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
-- Module      : Amazonka.MigrationHubStrategy.Types.RemoteSourceCodeAnalysisServerInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.RemoteSourceCodeAnalysisServerInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the server configured for source code analysis.
--
-- /See:/ 'newRemoteSourceCodeAnalysisServerInfo' smart constructor.
data RemoteSourceCodeAnalysisServerInfo = RemoteSourceCodeAnalysisServerInfo'
  { -- | The time when the remote source code server was configured.
    remoteSourceCodeAnalysisServerConfigurationTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoteSourceCodeAnalysisServerInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remoteSourceCodeAnalysisServerConfigurationTimestamp', 'remoteSourceCodeAnalysisServerInfo_remoteSourceCodeAnalysisServerConfigurationTimestamp' - The time when the remote source code server was configured.
newRemoteSourceCodeAnalysisServerInfo ::
  RemoteSourceCodeAnalysisServerInfo
newRemoteSourceCodeAnalysisServerInfo =
  RemoteSourceCodeAnalysisServerInfo'
    { remoteSourceCodeAnalysisServerConfigurationTimestamp =
        Prelude.Nothing
    }

-- | The time when the remote source code server was configured.
remoteSourceCodeAnalysisServerInfo_remoteSourceCodeAnalysisServerConfigurationTimestamp :: Lens.Lens' RemoteSourceCodeAnalysisServerInfo (Prelude.Maybe Prelude.Text)
remoteSourceCodeAnalysisServerInfo_remoteSourceCodeAnalysisServerConfigurationTimestamp = Lens.lens (\RemoteSourceCodeAnalysisServerInfo' {remoteSourceCodeAnalysisServerConfigurationTimestamp} -> remoteSourceCodeAnalysisServerConfigurationTimestamp) (\s@RemoteSourceCodeAnalysisServerInfo' {} a -> s {remoteSourceCodeAnalysisServerConfigurationTimestamp = a} :: RemoteSourceCodeAnalysisServerInfo)

instance
  Data.FromJSON
    RemoteSourceCodeAnalysisServerInfo
  where
  parseJSON =
    Data.withObject
      "RemoteSourceCodeAnalysisServerInfo"
      ( \x ->
          RemoteSourceCodeAnalysisServerInfo'
            Prelude.<$> ( x
                            Data..:? "remoteSourceCodeAnalysisServerConfigurationTimestamp"
                        )
      )

instance
  Prelude.Hashable
    RemoteSourceCodeAnalysisServerInfo
  where
  hashWithSalt
    _salt
    RemoteSourceCodeAnalysisServerInfo' {..} =
      _salt
        `Prelude.hashWithSalt` remoteSourceCodeAnalysisServerConfigurationTimestamp

instance
  Prelude.NFData
    RemoteSourceCodeAnalysisServerInfo
  where
  rnf RemoteSourceCodeAnalysisServerInfo' {..} =
    Prelude.rnf
      remoteSourceCodeAnalysisServerConfigurationTimestamp
