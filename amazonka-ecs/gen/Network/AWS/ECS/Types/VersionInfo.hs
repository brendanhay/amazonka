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
-- Module      : Network.AWS.ECS.Types.VersionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.VersionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Docker and Amazon ECS container agent version information about a
-- container instance.
--
-- /See:/ 'newVersionInfo' smart constructor.
data VersionInfo = VersionInfo'
  { -- | The version number of the Amazon ECS container agent.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The Docker version running on the container instance.
    dockerVersion :: Prelude.Maybe Prelude.Text,
    -- | The Git commit hash for the Amazon ECS container agent build on the
    -- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
    -- GitHub repository.
    agentHash :: Prelude.Maybe Prelude.Text
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
-- 'agentVersion', 'versionInfo_agentVersion' - The version number of the Amazon ECS container agent.
--
-- 'dockerVersion', 'versionInfo_dockerVersion' - The Docker version running on the container instance.
--
-- 'agentHash', 'versionInfo_agentHash' - The Git commit hash for the Amazon ECS container agent build on the
-- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
-- GitHub repository.
newVersionInfo ::
  VersionInfo
newVersionInfo =
  VersionInfo'
    { agentVersion = Prelude.Nothing,
      dockerVersion = Prelude.Nothing,
      agentHash = Prelude.Nothing
    }

-- | The version number of the Amazon ECS container agent.
versionInfo_agentVersion :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_agentVersion = Lens.lens (\VersionInfo' {agentVersion} -> agentVersion) (\s@VersionInfo' {} a -> s {agentVersion = a} :: VersionInfo)

-- | The Docker version running on the container instance.
versionInfo_dockerVersion :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_dockerVersion = Lens.lens (\VersionInfo' {dockerVersion} -> dockerVersion) (\s@VersionInfo' {} a -> s {dockerVersion = a} :: VersionInfo)

-- | The Git commit hash for the Amazon ECS container agent build on the
-- <https://github.com/aws/amazon-ecs-agent/commits/master amazon-ecs-agent>
-- GitHub repository.
versionInfo_agentHash :: Lens.Lens' VersionInfo (Prelude.Maybe Prelude.Text)
versionInfo_agentHash = Lens.lens (\VersionInfo' {agentHash} -> agentHash) (\s@VersionInfo' {} a -> s {agentHash = a} :: VersionInfo)

instance Core.FromJSON VersionInfo where
  parseJSON =
    Core.withObject
      "VersionInfo"
      ( \x ->
          VersionInfo'
            Prelude.<$> (x Core..:? "agentVersion")
            Prelude.<*> (x Core..:? "dockerVersion")
            Prelude.<*> (x Core..:? "agentHash")
      )

instance Prelude.Hashable VersionInfo

instance Prelude.NFData VersionInfo

instance Core.ToJSON VersionInfo where
  toJSON VersionInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("agentVersion" Core..=) Prelude.<$> agentVersion,
            ("dockerVersion" Core..=) Prelude.<$> dockerVersion,
            ("agentHash" Core..=) Prelude.<$> agentHash
          ]
      )
