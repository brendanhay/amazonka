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
-- Module      : Amazonka.SupportApp.Types.SlackWorkspaceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Types.SlackWorkspaceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a Slack workspace that you added to an Amazon Web
-- Services account.
--
-- /See:/ 'newSlackWorkspaceConfiguration' smart constructor.
data SlackWorkspaceConfiguration = SlackWorkspaceConfiguration'
  { -- | The team ID in Slack. This ID uniquely identifies a Slack workspace.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackWorkspaceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'teamId', 'slackWorkspaceConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace.
newSlackWorkspaceConfiguration ::
  -- | 'teamId'
  Prelude.Text ->
  SlackWorkspaceConfiguration
newSlackWorkspaceConfiguration pTeamId_ =
  SlackWorkspaceConfiguration' {teamId = pTeamId_}

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace.
slackWorkspaceConfiguration_teamId :: Lens.Lens' SlackWorkspaceConfiguration Prelude.Text
slackWorkspaceConfiguration_teamId = Lens.lens (\SlackWorkspaceConfiguration' {teamId} -> teamId) (\s@SlackWorkspaceConfiguration' {} a -> s {teamId = a} :: SlackWorkspaceConfiguration)

instance Core.FromJSON SlackWorkspaceConfiguration where
  parseJSON =
    Core.withObject
      "SlackWorkspaceConfiguration"
      ( \x ->
          SlackWorkspaceConfiguration'
            Prelude.<$> (x Core..: "teamId")
      )

instance Prelude.Hashable SlackWorkspaceConfiguration where
  hashWithSalt _salt SlackWorkspaceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` teamId

instance Prelude.NFData SlackWorkspaceConfiguration where
  rnf SlackWorkspaceConfiguration' {..} =
    Prelude.rnf teamId
