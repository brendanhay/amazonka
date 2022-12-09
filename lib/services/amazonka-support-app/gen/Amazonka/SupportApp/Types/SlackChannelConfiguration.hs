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
-- Module      : Amazonka.SupportApp.Types.SlackChannelConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SupportApp.Types.SlackChannelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SupportApp.Types.NotificationSeverityLevel

-- | The configuration for a Slack channel that you added for your Amazon Web
-- Services account.
--
-- /See:/ 'newSlackChannelConfiguration' smart constructor.
data SlackChannelConfiguration = SlackChannelConfiguration'
  { -- | The name of the Slack channel that you configured with the Amazon Web
    -- Services Support App for your Amazon Web Services account.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
    -- perform operations on Amazon Web Services. For more information, see
    -- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
    -- in the /Amazon Web Services Support User Guide/.
    channelRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Whether you want to get notified when a support case has a new
    -- correspondence.
    notifyOnAddCorrespondenceToCase :: Prelude.Maybe Prelude.Bool,
    -- | The case severity for a support case that you want to receive
    -- notifications.
    notifyOnCaseSeverity :: Prelude.Maybe NotificationSeverityLevel,
    -- | Whether you want to get notified when a support case is created or
    -- reopened.
    notifyOnCreateOrReopenCase :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to get notified when a support case is resolved.
    notifyOnResolveCase :: Prelude.Maybe Prelude.Bool,
    -- | The channel ID in Slack. This ID identifies a channel within a Slack
    -- workspace.
    channelId :: Prelude.Text,
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlackChannelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'slackChannelConfiguration_channelName' - The name of the Slack channel that you configured with the Amazon Web
-- Services Support App for your Amazon Web Services account.
--
-- 'channelRoleArn', 'slackChannelConfiguration_channelRoleArn' - The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
--
-- 'notifyOnAddCorrespondenceToCase', 'slackChannelConfiguration_notifyOnAddCorrespondenceToCase' - Whether you want to get notified when a support case has a new
-- correspondence.
--
-- 'notifyOnCaseSeverity', 'slackChannelConfiguration_notifyOnCaseSeverity' - The case severity for a support case that you want to receive
-- notifications.
--
-- 'notifyOnCreateOrReopenCase', 'slackChannelConfiguration_notifyOnCreateOrReopenCase' - Whether you want to get notified when a support case is created or
-- reopened.
--
-- 'notifyOnResolveCase', 'slackChannelConfiguration_notifyOnResolveCase' - Whether you want to get notified when a support case is resolved.
--
-- 'channelId', 'slackChannelConfiguration_channelId' - The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
--
-- 'teamId', 'slackChannelConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newSlackChannelConfiguration ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'teamId'
  Prelude.Text ->
  SlackChannelConfiguration
newSlackChannelConfiguration pChannelId_ pTeamId_ =
  SlackChannelConfiguration'
    { channelName =
        Prelude.Nothing,
      channelRoleArn = Prelude.Nothing,
      notifyOnAddCorrespondenceToCase =
        Prelude.Nothing,
      notifyOnCaseSeverity = Prelude.Nothing,
      notifyOnCreateOrReopenCase = Prelude.Nothing,
      notifyOnResolveCase = Prelude.Nothing,
      channelId = pChannelId_,
      teamId = pTeamId_
    }

-- | The name of the Slack channel that you configured with the Amazon Web
-- Services Support App for your Amazon Web Services account.
slackChannelConfiguration_channelName :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe Prelude.Text)
slackChannelConfiguration_channelName = Lens.lens (\SlackChannelConfiguration' {channelName} -> channelName) (\s@SlackChannelConfiguration' {} a -> s {channelName = a} :: SlackChannelConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
slackChannelConfiguration_channelRoleArn :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe Prelude.Text)
slackChannelConfiguration_channelRoleArn = Lens.lens (\SlackChannelConfiguration' {channelRoleArn} -> channelRoleArn) (\s@SlackChannelConfiguration' {} a -> s {channelRoleArn = a} :: SlackChannelConfiguration)

-- | Whether you want to get notified when a support case has a new
-- correspondence.
slackChannelConfiguration_notifyOnAddCorrespondenceToCase :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
slackChannelConfiguration_notifyOnAddCorrespondenceToCase = Lens.lens (\SlackChannelConfiguration' {notifyOnAddCorrespondenceToCase} -> notifyOnAddCorrespondenceToCase) (\s@SlackChannelConfiguration' {} a -> s {notifyOnAddCorrespondenceToCase = a} :: SlackChannelConfiguration)

-- | The case severity for a support case that you want to receive
-- notifications.
slackChannelConfiguration_notifyOnCaseSeverity :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe NotificationSeverityLevel)
slackChannelConfiguration_notifyOnCaseSeverity = Lens.lens (\SlackChannelConfiguration' {notifyOnCaseSeverity} -> notifyOnCaseSeverity) (\s@SlackChannelConfiguration' {} a -> s {notifyOnCaseSeverity = a} :: SlackChannelConfiguration)

-- | Whether you want to get notified when a support case is created or
-- reopened.
slackChannelConfiguration_notifyOnCreateOrReopenCase :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
slackChannelConfiguration_notifyOnCreateOrReopenCase = Lens.lens (\SlackChannelConfiguration' {notifyOnCreateOrReopenCase} -> notifyOnCreateOrReopenCase) (\s@SlackChannelConfiguration' {} a -> s {notifyOnCreateOrReopenCase = a} :: SlackChannelConfiguration)

-- | Whether you want to get notified when a support case is resolved.
slackChannelConfiguration_notifyOnResolveCase :: Lens.Lens' SlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
slackChannelConfiguration_notifyOnResolveCase = Lens.lens (\SlackChannelConfiguration' {notifyOnResolveCase} -> notifyOnResolveCase) (\s@SlackChannelConfiguration' {} a -> s {notifyOnResolveCase = a} :: SlackChannelConfiguration)

-- | The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
slackChannelConfiguration_channelId :: Lens.Lens' SlackChannelConfiguration Prelude.Text
slackChannelConfiguration_channelId = Lens.lens (\SlackChannelConfiguration' {channelId} -> channelId) (\s@SlackChannelConfiguration' {} a -> s {channelId = a} :: SlackChannelConfiguration)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
slackChannelConfiguration_teamId :: Lens.Lens' SlackChannelConfiguration Prelude.Text
slackChannelConfiguration_teamId = Lens.lens (\SlackChannelConfiguration' {teamId} -> teamId) (\s@SlackChannelConfiguration' {} a -> s {teamId = a} :: SlackChannelConfiguration)

instance Data.FromJSON SlackChannelConfiguration where
  parseJSON =
    Data.withObject
      "SlackChannelConfiguration"
      ( \x ->
          SlackChannelConfiguration'
            Prelude.<$> (x Data..:? "channelName")
            Prelude.<*> (x Data..:? "channelRoleArn")
            Prelude.<*> (x Data..:? "notifyOnAddCorrespondenceToCase")
            Prelude.<*> (x Data..:? "notifyOnCaseSeverity")
            Prelude.<*> (x Data..:? "notifyOnCreateOrReopenCase")
            Prelude.<*> (x Data..:? "notifyOnResolveCase")
            Prelude.<*> (x Data..: "channelId")
            Prelude.<*> (x Data..: "teamId")
      )

instance Prelude.Hashable SlackChannelConfiguration where
  hashWithSalt _salt SlackChannelConfiguration' {..} =
    _salt `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelRoleArn
      `Prelude.hashWithSalt` notifyOnAddCorrespondenceToCase
      `Prelude.hashWithSalt` notifyOnCaseSeverity
      `Prelude.hashWithSalt` notifyOnCreateOrReopenCase
      `Prelude.hashWithSalt` notifyOnResolveCase
      `Prelude.hashWithSalt` channelId
      `Prelude.hashWithSalt` teamId

instance Prelude.NFData SlackChannelConfiguration where
  rnf SlackChannelConfiguration' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelRoleArn
      `Prelude.seq` Prelude.rnf notifyOnAddCorrespondenceToCase
      `Prelude.seq` Prelude.rnf notifyOnCaseSeverity
      `Prelude.seq` Prelude.rnf notifyOnCreateOrReopenCase
      `Prelude.seq` Prelude.rnf notifyOnResolveCase
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf teamId
