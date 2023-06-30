{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SupportApp.UpdateSlackChannelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a Slack channel, such as case update
-- notifications.
module Amazonka.SupportApp.UpdateSlackChannelConfiguration
  ( -- * Creating a Request
    UpdateSlackChannelConfiguration (..),
    newUpdateSlackChannelConfiguration,

    -- * Request Lenses
    updateSlackChannelConfiguration_channelName,
    updateSlackChannelConfiguration_channelRoleArn,
    updateSlackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    updateSlackChannelConfiguration_notifyOnCaseSeverity,
    updateSlackChannelConfiguration_notifyOnCreateOrReopenCase,
    updateSlackChannelConfiguration_notifyOnResolveCase,
    updateSlackChannelConfiguration_channelId,
    updateSlackChannelConfiguration_teamId,

    -- * Destructuring the Response
    UpdateSlackChannelConfigurationResponse (..),
    newUpdateSlackChannelConfigurationResponse,

    -- * Response Lenses
    updateSlackChannelConfigurationResponse_channelId,
    updateSlackChannelConfigurationResponse_channelName,
    updateSlackChannelConfigurationResponse_channelRoleArn,
    updateSlackChannelConfigurationResponse_notifyOnAddCorrespondenceToCase,
    updateSlackChannelConfigurationResponse_notifyOnCaseSeverity,
    updateSlackChannelConfigurationResponse_notifyOnCreateOrReopenCase,
    updateSlackChannelConfigurationResponse_notifyOnResolveCase,
    updateSlackChannelConfigurationResponse_teamId,
    updateSlackChannelConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newUpdateSlackChannelConfiguration' smart constructor.
data UpdateSlackChannelConfiguration = UpdateSlackChannelConfiguration'
  { -- | The Slack channel name that you want to update.
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
    --
    -- If you specify @high@ or @all@, at least one of the following parameters
    -- must be @true@:
    --
    -- -   @notifyOnAddCorrespondenceToCase@
    --
    -- -   @notifyOnCreateOrReopenCase@
    --
    -- -   @notifyOnResolveCase@
    --
    -- If you specify @none@, any of the following parameters that you specify
    -- in your request must be @false@:
    --
    -- -   @notifyOnAddCorrespondenceToCase@
    --
    -- -   @notifyOnCreateOrReopenCase@
    --
    -- -   @notifyOnResolveCase@
    --
    -- If you don\'t specify these parameters in your request, the Amazon Web
    -- Services Support App uses the current values by default.
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
-- Create a value of 'UpdateSlackChannelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'updateSlackChannelConfiguration_channelName' - The Slack channel name that you want to update.
--
-- 'channelRoleArn', 'updateSlackChannelConfiguration_channelRoleArn' - The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
--
-- 'notifyOnAddCorrespondenceToCase', 'updateSlackChannelConfiguration_notifyOnAddCorrespondenceToCase' - Whether you want to get notified when a support case has a new
-- correspondence.
--
-- 'notifyOnCaseSeverity', 'updateSlackChannelConfiguration_notifyOnCaseSeverity' - The case severity for a support case that you want to receive
-- notifications.
--
-- If you specify @high@ or @all@, at least one of the following parameters
-- must be @true@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you specify @none@, any of the following parameters that you specify
-- in your request must be @false@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you don\'t specify these parameters in your request, the Amazon Web
-- Services Support App uses the current values by default.
--
-- 'notifyOnCreateOrReopenCase', 'updateSlackChannelConfiguration_notifyOnCreateOrReopenCase' - Whether you want to get notified when a support case is created or
-- reopened.
--
-- 'notifyOnResolveCase', 'updateSlackChannelConfiguration_notifyOnResolveCase' - Whether you want to get notified when a support case is resolved.
--
-- 'channelId', 'updateSlackChannelConfiguration_channelId' - The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
--
-- 'teamId', 'updateSlackChannelConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newUpdateSlackChannelConfiguration ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'teamId'
  Prelude.Text ->
  UpdateSlackChannelConfiguration
newUpdateSlackChannelConfiguration
  pChannelId_
  pTeamId_ =
    UpdateSlackChannelConfiguration'
      { channelName =
          Prelude.Nothing,
        channelRoleArn = Prelude.Nothing,
        notifyOnAddCorrespondenceToCase =
          Prelude.Nothing,
        notifyOnCaseSeverity = Prelude.Nothing,
        notifyOnCreateOrReopenCase =
          Prelude.Nothing,
        notifyOnResolveCase = Prelude.Nothing,
        channelId = pChannelId_,
        teamId = pTeamId_
      }

-- | The Slack channel name that you want to update.
updateSlackChannelConfiguration_channelName :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe Prelude.Text)
updateSlackChannelConfiguration_channelName = Lens.lens (\UpdateSlackChannelConfiguration' {channelName} -> channelName) (\s@UpdateSlackChannelConfiguration' {} a -> s {channelName = a} :: UpdateSlackChannelConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
updateSlackChannelConfiguration_channelRoleArn :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe Prelude.Text)
updateSlackChannelConfiguration_channelRoleArn = Lens.lens (\UpdateSlackChannelConfiguration' {channelRoleArn} -> channelRoleArn) (\s@UpdateSlackChannelConfiguration' {} a -> s {channelRoleArn = a} :: UpdateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case has a new
-- correspondence.
updateSlackChannelConfiguration_notifyOnAddCorrespondenceToCase :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfiguration_notifyOnAddCorrespondenceToCase = Lens.lens (\UpdateSlackChannelConfiguration' {notifyOnAddCorrespondenceToCase} -> notifyOnAddCorrespondenceToCase) (\s@UpdateSlackChannelConfiguration' {} a -> s {notifyOnAddCorrespondenceToCase = a} :: UpdateSlackChannelConfiguration)

-- | The case severity for a support case that you want to receive
-- notifications.
--
-- If you specify @high@ or @all@, at least one of the following parameters
-- must be @true@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you specify @none@, any of the following parameters that you specify
-- in your request must be @false@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you don\'t specify these parameters in your request, the Amazon Web
-- Services Support App uses the current values by default.
updateSlackChannelConfiguration_notifyOnCaseSeverity :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe NotificationSeverityLevel)
updateSlackChannelConfiguration_notifyOnCaseSeverity = Lens.lens (\UpdateSlackChannelConfiguration' {notifyOnCaseSeverity} -> notifyOnCaseSeverity) (\s@UpdateSlackChannelConfiguration' {} a -> s {notifyOnCaseSeverity = a} :: UpdateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case is created or
-- reopened.
updateSlackChannelConfiguration_notifyOnCreateOrReopenCase :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfiguration_notifyOnCreateOrReopenCase = Lens.lens (\UpdateSlackChannelConfiguration' {notifyOnCreateOrReopenCase} -> notifyOnCreateOrReopenCase) (\s@UpdateSlackChannelConfiguration' {} a -> s {notifyOnCreateOrReopenCase = a} :: UpdateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case is resolved.
updateSlackChannelConfiguration_notifyOnResolveCase :: Lens.Lens' UpdateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfiguration_notifyOnResolveCase = Lens.lens (\UpdateSlackChannelConfiguration' {notifyOnResolveCase} -> notifyOnResolveCase) (\s@UpdateSlackChannelConfiguration' {} a -> s {notifyOnResolveCase = a} :: UpdateSlackChannelConfiguration)

-- | The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
updateSlackChannelConfiguration_channelId :: Lens.Lens' UpdateSlackChannelConfiguration Prelude.Text
updateSlackChannelConfiguration_channelId = Lens.lens (\UpdateSlackChannelConfiguration' {channelId} -> channelId) (\s@UpdateSlackChannelConfiguration' {} a -> s {channelId = a} :: UpdateSlackChannelConfiguration)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
updateSlackChannelConfiguration_teamId :: Lens.Lens' UpdateSlackChannelConfiguration Prelude.Text
updateSlackChannelConfiguration_teamId = Lens.lens (\UpdateSlackChannelConfiguration' {teamId} -> teamId) (\s@UpdateSlackChannelConfiguration' {} a -> s {teamId = a} :: UpdateSlackChannelConfiguration)

instance
  Core.AWSRequest
    UpdateSlackChannelConfiguration
  where
  type
    AWSResponse UpdateSlackChannelConfiguration =
      UpdateSlackChannelConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSlackChannelConfigurationResponse'
            Prelude.<$> (x Data..?> "channelId")
            Prelude.<*> (x Data..?> "channelName")
            Prelude.<*> (x Data..?> "channelRoleArn")
            Prelude.<*> (x Data..?> "notifyOnAddCorrespondenceToCase")
            Prelude.<*> (x Data..?> "notifyOnCaseSeverity")
            Prelude.<*> (x Data..?> "notifyOnCreateOrReopenCase")
            Prelude.<*> (x Data..?> "notifyOnResolveCase")
            Prelude.<*> (x Data..?> "teamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSlackChannelConfiguration
  where
  hashWithSalt
    _salt
    UpdateSlackChannelConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` channelName
        `Prelude.hashWithSalt` channelRoleArn
        `Prelude.hashWithSalt` notifyOnAddCorrespondenceToCase
        `Prelude.hashWithSalt` notifyOnCaseSeverity
        `Prelude.hashWithSalt` notifyOnCreateOrReopenCase
        `Prelude.hashWithSalt` notifyOnResolveCase
        `Prelude.hashWithSalt` channelId
        `Prelude.hashWithSalt` teamId

instance
  Prelude.NFData
    UpdateSlackChannelConfiguration
  where
  rnf UpdateSlackChannelConfiguration' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelRoleArn
      `Prelude.seq` Prelude.rnf notifyOnAddCorrespondenceToCase
      `Prelude.seq` Prelude.rnf notifyOnCaseSeverity
      `Prelude.seq` Prelude.rnf notifyOnCreateOrReopenCase
      `Prelude.seq` Prelude.rnf notifyOnResolveCase
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf teamId

instance
  Data.ToHeaders
    UpdateSlackChannelConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSlackChannelConfiguration where
  toJSON UpdateSlackChannelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channelName" Data..=) Prelude.<$> channelName,
            ("channelRoleArn" Data..=)
              Prelude.<$> channelRoleArn,
            ("notifyOnAddCorrespondenceToCase" Data..=)
              Prelude.<$> notifyOnAddCorrespondenceToCase,
            ("notifyOnCaseSeverity" Data..=)
              Prelude.<$> notifyOnCaseSeverity,
            ("notifyOnCreateOrReopenCase" Data..=)
              Prelude.<$> notifyOnCreateOrReopenCase,
            ("notifyOnResolveCase" Data..=)
              Prelude.<$> notifyOnResolveCase,
            Prelude.Just ("channelId" Data..= channelId),
            Prelude.Just ("teamId" Data..= teamId)
          ]
      )

instance Data.ToPath UpdateSlackChannelConfiguration where
  toPath =
    Prelude.const
      "/control/update-slack-channel-configuration"

instance Data.ToQuery UpdateSlackChannelConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSlackChannelConfigurationResponse' smart constructor.
data UpdateSlackChannelConfigurationResponse = UpdateSlackChannelConfigurationResponse'
  { -- | The channel ID in Slack. This ID identifies a channel within a Slack
    -- workspace.
    channelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Slack channel that you configure for the Amazon Web
    -- Services Support App.
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
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSlackChannelConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'updateSlackChannelConfigurationResponse_channelId' - The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
--
-- 'channelName', 'updateSlackChannelConfigurationResponse_channelName' - The name of the Slack channel that you configure for the Amazon Web
-- Services Support App.
--
-- 'channelRoleArn', 'updateSlackChannelConfigurationResponse_channelRoleArn' - The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
--
-- 'notifyOnAddCorrespondenceToCase', 'updateSlackChannelConfigurationResponse_notifyOnAddCorrespondenceToCase' - Whether you want to get notified when a support case has a new
-- correspondence.
--
-- 'notifyOnCaseSeverity', 'updateSlackChannelConfigurationResponse_notifyOnCaseSeverity' - The case severity for a support case that you want to receive
-- notifications.
--
-- 'notifyOnCreateOrReopenCase', 'updateSlackChannelConfigurationResponse_notifyOnCreateOrReopenCase' - Whether you want to get notified when a support case is created or
-- reopened.
--
-- 'notifyOnResolveCase', 'updateSlackChannelConfigurationResponse_notifyOnResolveCase' - Whether you want to get notified when a support case is resolved.
--
-- 'teamId', 'updateSlackChannelConfigurationResponse_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
--
-- 'httpStatus', 'updateSlackChannelConfigurationResponse_httpStatus' - The response's http status code.
newUpdateSlackChannelConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSlackChannelConfigurationResponse
newUpdateSlackChannelConfigurationResponse
  pHttpStatus_ =
    UpdateSlackChannelConfigurationResponse'
      { channelId =
          Prelude.Nothing,
        channelName = Prelude.Nothing,
        channelRoleArn = Prelude.Nothing,
        notifyOnAddCorrespondenceToCase =
          Prelude.Nothing,
        notifyOnCaseSeverity =
          Prelude.Nothing,
        notifyOnCreateOrReopenCase =
          Prelude.Nothing,
        notifyOnResolveCase =
          Prelude.Nothing,
        teamId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
updateSlackChannelConfigurationResponse_channelId :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Text)
updateSlackChannelConfigurationResponse_channelId = Lens.lens (\UpdateSlackChannelConfigurationResponse' {channelId} -> channelId) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {channelId = a} :: UpdateSlackChannelConfigurationResponse)

-- | The name of the Slack channel that you configure for the Amazon Web
-- Services Support App.
updateSlackChannelConfigurationResponse_channelName :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Text)
updateSlackChannelConfigurationResponse_channelName = Lens.lens (\UpdateSlackChannelConfigurationResponse' {channelName} -> channelName) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {channelName = a} :: UpdateSlackChannelConfigurationResponse)

-- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
updateSlackChannelConfigurationResponse_channelRoleArn :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Text)
updateSlackChannelConfigurationResponse_channelRoleArn = Lens.lens (\UpdateSlackChannelConfigurationResponse' {channelRoleArn} -> channelRoleArn) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {channelRoleArn = a} :: UpdateSlackChannelConfigurationResponse)

-- | Whether you want to get notified when a support case has a new
-- correspondence.
updateSlackChannelConfigurationResponse_notifyOnAddCorrespondenceToCase :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfigurationResponse_notifyOnAddCorrespondenceToCase = Lens.lens (\UpdateSlackChannelConfigurationResponse' {notifyOnAddCorrespondenceToCase} -> notifyOnAddCorrespondenceToCase) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {notifyOnAddCorrespondenceToCase = a} :: UpdateSlackChannelConfigurationResponse)

-- | The case severity for a support case that you want to receive
-- notifications.
updateSlackChannelConfigurationResponse_notifyOnCaseSeverity :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe NotificationSeverityLevel)
updateSlackChannelConfigurationResponse_notifyOnCaseSeverity = Lens.lens (\UpdateSlackChannelConfigurationResponse' {notifyOnCaseSeverity} -> notifyOnCaseSeverity) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {notifyOnCaseSeverity = a} :: UpdateSlackChannelConfigurationResponse)

-- | Whether you want to get notified when a support case is created or
-- reopened.
updateSlackChannelConfigurationResponse_notifyOnCreateOrReopenCase :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfigurationResponse_notifyOnCreateOrReopenCase = Lens.lens (\UpdateSlackChannelConfigurationResponse' {notifyOnCreateOrReopenCase} -> notifyOnCreateOrReopenCase) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {notifyOnCreateOrReopenCase = a} :: UpdateSlackChannelConfigurationResponse)

-- | Whether you want to get notified when a support case is resolved.
updateSlackChannelConfigurationResponse_notifyOnResolveCase :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Bool)
updateSlackChannelConfigurationResponse_notifyOnResolveCase = Lens.lens (\UpdateSlackChannelConfigurationResponse' {notifyOnResolveCase} -> notifyOnResolveCase) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {notifyOnResolveCase = a} :: UpdateSlackChannelConfigurationResponse)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
updateSlackChannelConfigurationResponse_teamId :: Lens.Lens' UpdateSlackChannelConfigurationResponse (Prelude.Maybe Prelude.Text)
updateSlackChannelConfigurationResponse_teamId = Lens.lens (\UpdateSlackChannelConfigurationResponse' {teamId} -> teamId) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {teamId = a} :: UpdateSlackChannelConfigurationResponse)

-- | The response's http status code.
updateSlackChannelConfigurationResponse_httpStatus :: Lens.Lens' UpdateSlackChannelConfigurationResponse Prelude.Int
updateSlackChannelConfigurationResponse_httpStatus = Lens.lens (\UpdateSlackChannelConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateSlackChannelConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateSlackChannelConfigurationResponse)

instance
  Prelude.NFData
    UpdateSlackChannelConfigurationResponse
  where
  rnf UpdateSlackChannelConfigurationResponse' {..} =
    Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelRoleArn
      `Prelude.seq` Prelude.rnf notifyOnAddCorrespondenceToCase
      `Prelude.seq` Prelude.rnf notifyOnCaseSeverity
      `Prelude.seq` Prelude.rnf notifyOnCreateOrReopenCase
      `Prelude.seq` Prelude.rnf notifyOnResolveCase
      `Prelude.seq` Prelude.rnf teamId
      `Prelude.seq` Prelude.rnf httpStatus
