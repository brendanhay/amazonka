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
-- Module      : Amazonka.ECS.Types.VersionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.VersionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Docker and Amazon ECS container agent version information about a
-- container instance.
--
-- /See:/ 'newVersionInfo' smart constructor.
data VersionInfo = VersionInfo'
  { -- | The Git commit hash for the Amazon ECS container agent build on the
    -- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
    -- GitHub repository.
    agentHash :: Prelude.Maybe Prelude.Text,
    -- | The version number of the Amazon ECS container agent.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Docker version that\'s running on the container instance.
    dockerVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentHash', 'versionInfo_agentHash' - The Git commit hash for the Amazon ECS container agent build on the
-- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
-- GitHub repository.
--
-- 'agentVersion', 'versionInfo_agentVersion' - The version number of the Amazon ECS container agent.
--
-- 'dockerVersion', 'versionInfo_dockerVersion' - The Docker version that\'s running on the container instance.
newVersionInfo ::
  VersionInfo
newVersionInfo =
  VersionInfo'
    { agentHash = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      dockerVersion = Prelude.Nothing
    }

-- | The Git commit hash for the Amazon ECS container agent build on the
-- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
-- GitHub repository.
versionInfo_agentHash :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_agentHash = Lens.lens (\VersionInfo' {agentHash} -> agentHash) (\s@VersionInfo' {} a -> s {agentHash = a} :: VersionInfo)

-- | The version number of the Amazon ECS container agent.
versionInfo_agentVersion :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_agentVersion = Lens.lens (\VersionInfo' {agentVersion} -> agentVersion) (\s@VersionInfo' {} a -> s {agentVersion = a} :: VersionInfo)

-- | The Docker version that\'s running on the container instance.
versionInfo_dockerVersion :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_dockerVersion = Lens.lens (\VersionInfo' {dockerVersion} -> dockerVersion) (\s@VersionInfo' {} a -> s {dockerVersion = a} :: VersionInfo)

instance Data.FromJSON VersionInfo where
  parseJSON =
    Data.withObject
      "VersionInfo"
      ( \x ->
          VersionInfo'
            Prelude.<$> (x Data..:? "agentHash")
            Prelude.<*> (x Data..:? "agentVersion")
            Prelude.<*> (x Data..:? "dockerVersion")
      )

instance Prelude.Hashable VersionInfo where
  hashWithSalt _salt VersionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` agentHash
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` dockerVersion

instance Prelude.NFData VersionInfo where
  rnf VersionInfo' {..} =
    Prelude.rnf agentHash
      `Prelude.seq` Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf dockerVersion

instance Data.ToJSON VersionInfo where
  toJSON VersionInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("agentHash" Data..=) Prelude.<$> agentHash,
            ("agentVersion" Data..=) Prelude.<$> agentVersion,
            ("dockerVersion" Data..=) Prelude.<$> dockerVersion
          ]
      )
