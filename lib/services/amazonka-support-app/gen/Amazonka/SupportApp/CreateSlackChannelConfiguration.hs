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
-- Module      : Amazonka.SupportApp.CreateSlackChannelConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Slack channel configuration for your Amazon Web Services
-- account.
--
-- -   You can add up to 5 Slack workspaces for your account.
--
-- -   You can add up to 20 Slack channels for your account.
--
-- A Slack channel can have up to 100 Amazon Web Services accounts. This
-- means that only 100 accounts can add the same Slack channel to the
-- Amazon Web Services Support App. We recommend that you only add the
-- accounts that you need to manage support cases for your organization.
-- This can reduce the notifications about case updates that you receive in
-- the Slack channel.
--
-- We recommend that you choose a private Slack channel so that only
-- members in that channel have read and write access to your support
-- cases. Anyone in your Slack channel can create, update, or resolve
-- support cases for your account. Users require an invitation to join
-- private channels.
module Amazonka.SupportApp.CreateSlackChannelConfiguration
  ( -- * Creating a Request
    CreateSlackChannelConfiguration (..),
    newCreateSlackChannelConfiguration,

    -- * Request Lenses
    createSlackChannelConfiguration_channelName,
    createSlackChannelConfiguration_notifyOnAddCorrespondenceToCase,
    createSlackChannelConfiguration_notifyOnCreateOrReopenCase,
    createSlackChannelConfiguration_notifyOnResolveCase,
    createSlackChannelConfiguration_channelId,
    createSlackChannelConfiguration_channelRoleArn,
    createSlackChannelConfiguration_notifyOnCaseSeverity,
    createSlackChannelConfiguration_teamId,

    -- * Destructuring the Response
    CreateSlackChannelConfigurationResponse (..),
    newCreateSlackChannelConfigurationResponse,

    -- * Response Lenses
    createSlackChannelConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newCreateSlackChannelConfiguration' smart constructor.
data CreateSlackChannelConfiguration = CreateSlackChannelConfiguration'
  { -- | The name of the Slack channel that you configure for the Amazon Web
    -- Services Support App.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | Whether you want to get notified when a support case has a new
    -- correspondence.
    notifyOnAddCorrespondenceToCase :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to get notified when a support case is created or
    -- reopened.
    notifyOnCreateOrReopenCase :: Prelude.Maybe Prelude.Bool,
    -- | Whether you want to get notified when a support case is resolved.
    notifyOnResolveCase :: Prelude.Maybe Prelude.Bool,
    -- | The channel ID in Slack. This ID identifies a channel within a Slack
    -- workspace.
    channelId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
    -- perform operations on Amazon Web Services. For more information, see
    -- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
    -- in the /Amazon Web Services Support User Guide/.
    channelRoleArn :: Prelude.Text,
    -- | The case severity for a support case that you want to receive
    -- notifications.
    --
    -- If you specify @high@ or @all@, you must specify @true@ for at least one
    -- of the following parameters:
    --
    -- -   @notifyOnAddCorrespondenceToCase@
    --
    -- -   @notifyOnCreateOrReopenCase@
    --
    -- -   @notifyOnResolveCase@
    --
    -- If you specify @none@, the following parameters must be null or @false@:
    --
    -- -   @notifyOnAddCorrespondenceToCase@
    --
    -- -   @notifyOnCreateOrReopenCase@
    --
    -- -   @notifyOnResolveCase@
    --
    -- If you don\'t specify these parameters in your request, they default to
    -- @false@.
    notifyOnCaseSeverity :: NotificationSeverityLevel,
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlackChannelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'createSlackChannelConfiguration_channelName' - The name of the Slack channel that you configure for the Amazon Web
-- Services Support App.
--
-- 'notifyOnAddCorrespondenceToCase', 'createSlackChannelConfiguration_notifyOnAddCorrespondenceToCase' - Whether you want to get notified when a support case has a new
-- correspondence.
--
-- 'notifyOnCreateOrReopenCase', 'createSlackChannelConfiguration_notifyOnCreateOrReopenCase' - Whether you want to get notified when a support case is created or
-- reopened.
--
-- 'notifyOnResolveCase', 'createSlackChannelConfiguration_notifyOnResolveCase' - Whether you want to get notified when a support case is resolved.
--
-- 'channelId', 'createSlackChannelConfiguration_channelId' - The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
--
-- 'channelRoleArn', 'createSlackChannelConfiguration_channelRoleArn' - The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
--
-- 'notifyOnCaseSeverity', 'createSlackChannelConfiguration_notifyOnCaseSeverity' - The case severity for a support case that you want to receive
-- notifications.
--
-- If you specify @high@ or @all@, you must specify @true@ for at least one
-- of the following parameters:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you specify @none@, the following parameters must be null or @false@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you don\'t specify these parameters in your request, they default to
-- @false@.
--
-- 'teamId', 'createSlackChannelConfiguration_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
newCreateSlackChannelConfiguration ::
  -- | 'channelId'
  Prelude.Text ->
  -- | 'channelRoleArn'
  Prelude.Text ->
  -- | 'notifyOnCaseSeverity'
  NotificationSeverityLevel ->
  -- | 'teamId'
  Prelude.Text ->
  CreateSlackChannelConfiguration
newCreateSlackChannelConfiguration
  pChannelId_
  pChannelRoleArn_
  pNotifyOnCaseSeverity_
  pTeamId_ =
    CreateSlackChannelConfiguration'
      { channelName =
          Prelude.Nothing,
        notifyOnAddCorrespondenceToCase =
          Prelude.Nothing,
        notifyOnCreateOrReopenCase =
          Prelude.Nothing,
        notifyOnResolveCase = Prelude.Nothing,
        channelId = pChannelId_,
        channelRoleArn = pChannelRoleArn_,
        notifyOnCaseSeverity =
          pNotifyOnCaseSeverity_,
        teamId = pTeamId_
      }

-- | The name of the Slack channel that you configure for the Amazon Web
-- Services Support App.
createSlackChannelConfiguration_channelName :: Lens.Lens' CreateSlackChannelConfiguration (Prelude.Maybe Prelude.Text)
createSlackChannelConfiguration_channelName = Lens.lens (\CreateSlackChannelConfiguration' {channelName} -> channelName) (\s@CreateSlackChannelConfiguration' {} a -> s {channelName = a} :: CreateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case has a new
-- correspondence.
createSlackChannelConfiguration_notifyOnAddCorrespondenceToCase :: Lens.Lens' CreateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
createSlackChannelConfiguration_notifyOnAddCorrespondenceToCase = Lens.lens (\CreateSlackChannelConfiguration' {notifyOnAddCorrespondenceToCase} -> notifyOnAddCorrespondenceToCase) (\s@CreateSlackChannelConfiguration' {} a -> s {notifyOnAddCorrespondenceToCase = a} :: CreateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case is created or
-- reopened.
createSlackChannelConfiguration_notifyOnCreateOrReopenCase :: Lens.Lens' CreateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
createSlackChannelConfiguration_notifyOnCreateOrReopenCase = Lens.lens (\CreateSlackChannelConfiguration' {notifyOnCreateOrReopenCase} -> notifyOnCreateOrReopenCase) (\s@CreateSlackChannelConfiguration' {} a -> s {notifyOnCreateOrReopenCase = a} :: CreateSlackChannelConfiguration)

-- | Whether you want to get notified when a support case is resolved.
createSlackChannelConfiguration_notifyOnResolveCase :: Lens.Lens' CreateSlackChannelConfiguration (Prelude.Maybe Prelude.Bool)
createSlackChannelConfiguration_notifyOnResolveCase = Lens.lens (\CreateSlackChannelConfiguration' {notifyOnResolveCase} -> notifyOnResolveCase) (\s@CreateSlackChannelConfiguration' {} a -> s {notifyOnResolveCase = a} :: CreateSlackChannelConfiguration)

-- | The channel ID in Slack. This ID identifies a channel within a Slack
-- workspace.
createSlackChannelConfiguration_channelId :: Lens.Lens' CreateSlackChannelConfiguration Prelude.Text
createSlackChannelConfiguration_channelId = Lens.lens (\CreateSlackChannelConfiguration' {channelId} -> channelId) (\s@CreateSlackChannelConfiguration' {} a -> s {channelId = a} :: CreateSlackChannelConfiguration)

-- | The Amazon Resource Name (ARN) of an IAM role that you want to use to
-- perform operations on Amazon Web Services. For more information, see
-- <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>
-- in the /Amazon Web Services Support User Guide/.
createSlackChannelConfiguration_channelRoleArn :: Lens.Lens' CreateSlackChannelConfiguration Prelude.Text
createSlackChannelConfiguration_channelRoleArn = Lens.lens (\CreateSlackChannelConfiguration' {channelRoleArn} -> channelRoleArn) (\s@CreateSlackChannelConfiguration' {} a -> s {channelRoleArn = a} :: CreateSlackChannelConfiguration)

-- | The case severity for a support case that you want to receive
-- notifications.
--
-- If you specify @high@ or @all@, you must specify @true@ for at least one
-- of the following parameters:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you specify @none@, the following parameters must be null or @false@:
--
-- -   @notifyOnAddCorrespondenceToCase@
--
-- -   @notifyOnCreateOrReopenCase@
--
-- -   @notifyOnResolveCase@
--
-- If you don\'t specify these parameters in your request, they default to
-- @false@.
createSlackChannelConfiguration_notifyOnCaseSeverity :: Lens.Lens' CreateSlackChannelConfiguration NotificationSeverityLevel
createSlackChannelConfiguration_notifyOnCaseSeverity = Lens.lens (\CreateSlackChannelConfiguration' {notifyOnCaseSeverity} -> notifyOnCaseSeverity) (\s@CreateSlackChannelConfiguration' {} a -> s {notifyOnCaseSeverity = a} :: CreateSlackChannelConfiguration)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
createSlackChannelConfiguration_teamId :: Lens.Lens' CreateSlackChannelConfiguration Prelude.Text
createSlackChannelConfiguration_teamId = Lens.lens (\CreateSlackChannelConfiguration' {teamId} -> teamId) (\s@CreateSlackChannelConfiguration' {} a -> s {teamId = a} :: CreateSlackChannelConfiguration)

instance
  Core.AWSRequest
    CreateSlackChannelConfiguration
  where
  type
    AWSResponse CreateSlackChannelConfiguration =
      CreateSlackChannelConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateSlackChannelConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSlackChannelConfiguration
  where
  hashWithSalt
    _salt
    CreateSlackChannelConfiguration' {..} =
      _salt `Prelude.hashWithSalt` channelName
        `Prelude.hashWithSalt` notifyOnAddCorrespondenceToCase
        `Prelude.hashWithSalt` notifyOnCreateOrReopenCase
        `Prelude.hashWithSalt` notifyOnResolveCase
        `Prelude.hashWithSalt` channelId
        `Prelude.hashWithSalt` channelRoleArn
        `Prelude.hashWithSalt` notifyOnCaseSeverity
        `Prelude.hashWithSalt` teamId

instance
  Prelude.NFData
    CreateSlackChannelConfiguration
  where
  rnf CreateSlackChannelConfiguration' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf notifyOnAddCorrespondenceToCase
      `Prelude.seq` Prelude.rnf notifyOnCreateOrReopenCase
      `Prelude.seq` Prelude.rnf notifyOnResolveCase
      `Prelude.seq` Prelude.rnf channelId
      `Prelude.seq` Prelude.rnf channelRoleArn
      `Prelude.seq` Prelude.rnf notifyOnCaseSeverity
      `Prelude.seq` Prelude.rnf teamId

instance
  Data.ToHeaders
    CreateSlackChannelConfiguration
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

instance Data.ToJSON CreateSlackChannelConfiguration where
  toJSON CreateSlackChannelConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channelName" Data..=) Prelude.<$> channelName,
            ("notifyOnAddCorrespondenceToCase" Data..=)
              Prelude.<$> notifyOnAddCorrespondenceToCase,
            ("notifyOnCreateOrReopenCase" Data..=)
              Prelude.<$> notifyOnCreateOrReopenCase,
            ("notifyOnResolveCase" Data..=)
              Prelude.<$> notifyOnResolveCase,
            Prelude.Just ("channelId" Data..= channelId),
            Prelude.Just
              ("channelRoleArn" Data..= channelRoleArn),
            Prelude.Just
              ( "notifyOnCaseSeverity"
                  Data..= notifyOnCaseSeverity
              ),
            Prelude.Just ("teamId" Data..= teamId)
          ]
      )

instance Data.ToPath CreateSlackChannelConfiguration where
  toPath =
    Prelude.const
      "/control/create-slack-channel-configuration"

instance Data.ToQuery CreateSlackChannelConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSlackChannelConfigurationResponse' smart constructor.
data CreateSlackChannelConfigurationResponse = CreateSlackChannelConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSlackChannelConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSlackChannelConfigurationResponse_httpStatus' - The response's http status code.
newCreateSlackChannelConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSlackChannelConfigurationResponse
newCreateSlackChannelConfigurationResponse
  pHttpStatus_ =
    CreateSlackChannelConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createSlackChannelConfigurationResponse_httpStatus :: Lens.Lens' CreateSlackChannelConfigurationResponse Prelude.Int
createSlackChannelConfigurationResponse_httpStatus = Lens.lens (\CreateSlackChannelConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateSlackChannelConfigurationResponse' {} a -> s {httpStatus = a} :: CreateSlackChannelConfigurationResponse)

instance
  Prelude.NFData
    CreateSlackChannelConfigurationResponse
  where
  rnf CreateSlackChannelConfigurationResponse' {..} =
    Prelude.rnf httpStatus
