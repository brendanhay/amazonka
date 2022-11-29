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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Types.SlackWorkspaceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a Slack workspace that you added to an Amazon Web
-- Services account.
--
-- /See:/ 'newSlackWorkspaceConfiguration' smart constructor.
data SlackWorkspaceConfiguration = SlackWorkspaceConfiguration'
  { -- | The name of the Slack workspace.
    teamName :: Prelude.Maybe Prelude.Text,
    -- | Whether to allow member accounts to authorize Slack workspaces. Member
    -- accounts must be part of an organization in Organizations.
    allowOrganizationMemberAccount :: Prelude.Maybe Prelude.Bool,
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
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
-- 'teamName', 'slackWorkspaceConfiguration_teamName' - The name of the Slack workspace.
--
-- 'allowOrganizationMemberAccount', 'slackWorkspaceConfiguration_allowOrganizationMemberAccount' - Whether to allow member accounts to authorize Slack workspaces. Member
-- accounts must be part of an organization in Organizations.
--
-- 'teamId', 'slackWorkspaceConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newSlackWorkspaceConfiguration ::
  -- | 'teamId'
  Prelude.Text ->
  SlackWorkspaceConfiguration
newSlackWorkspaceConfiguration pTeamId_ =
  SlackWorkspaceConfiguration'
    { teamName =
        Prelude.Nothing,
      allowOrganizationMemberAccount =
        Prelude.Nothing,
      teamId = pTeamId_
    }

-- | The name of the Slack workspace.
slackWorkspaceConfiguration_teamName :: Lens.Lens' SlackWorkspaceConfiguration (Prelude.Maybe Prelude.Text)
slackWorkspaceConfiguration_teamName = Lens.lens (\SlackWorkspaceConfiguration' {teamName} -> teamName) (\s@SlackWorkspaceConfiguration' {} a -> s {teamName = a} :: SlackWorkspaceConfiguration)

-- | Whether to allow member accounts to authorize Slack workspaces. Member
-- accounts must be part of an organization in Organizations.
slackWorkspaceConfiguration_allowOrganizationMemberAccount :: Lens.Lens' SlackWorkspaceConfiguration (Prelude.Maybe Prelude.Bool)
slackWorkspaceConfiguration_allowOrganizationMemberAccount = Lens.lens (\SlackWorkspaceConfiguration' {allowOrganizationMemberAccount} -> allowOrganizationMemberAccount) (\s@SlackWorkspaceConfiguration' {} a -> s {allowOrganizationMemberAccount = a} :: SlackWorkspaceConfiguration)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
slackWorkspaceConfiguration_teamId :: Lens.Lens' SlackWorkspaceConfiguration Prelude.Text
slackWorkspaceConfiguration_teamId = Lens.lens (\SlackWorkspaceConfiguration' {teamId} -> teamId) (\s@SlackWorkspaceConfiguration' {} a -> s {teamId = a} :: SlackWorkspaceConfiguration)

instance Core.FromJSON SlackWorkspaceConfiguration where
  parseJSON =
    Core.withObject
      "SlackWorkspaceConfiguration"
      ( \x ->
          SlackWorkspaceConfiguration'
            Prelude.<$> (x Core..:? "teamName")
            Prelude.<*> (x Core..:? "allowOrganizationMemberAccount")
            Prelude.<*> (x Core..: "teamId")
      )

instance Prelude.Hashable SlackWorkspaceConfiguration where
  hashWithSalt _salt SlackWorkspaceConfiguration' {..} =
    _salt `Prelude.hashWithSalt` teamName
      `Prelude.hashWithSalt` allowOrganizationMemberAccount
      `Prelude.hashWithSalt` teamId

instance Prelude.NFData SlackWorkspaceConfiguration where
  rnf SlackWorkspaceConfiguration' {..} =
    Prelude.rnf teamName
      `Prelude.seq` Prelude.rnf allowOrganizationMemberAccount
      `Prelude.seq` Prelude.rnf teamId
