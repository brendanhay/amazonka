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
-- Module      : Amazonka.SupportApp.RegisterSlackWorkspaceForOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a Slack workspace for your Amazon Web Services account. To
-- call this API, your account must be part of an organization in
-- Organizations.
--
-- If you\'re the /management account/ and you want to register Slack
-- workspaces for your organization, you must complete the following tasks:
--
-- 1.  Sign in to the
--     <https://console.aws.amazon.com/support/app Amazon Web Services Support Center>
--     and authorize the Slack workspaces where you want your organization
--     to have access to. See
--     <https://docs.aws.amazon.com/awssupport/latest/user/authorize-slack-workspace.html Authorize a Slack workspace>
--     in the /Amazon Web Services Support User Guide/.
--
-- 2.  Call the @RegisterSlackWorkspaceForOrganization@ API to authorize
--     each Slack workspace for the organization.
--
-- After the management account authorizes the Slack workspace, member
-- accounts can call this API to authorize the same Slack workspace for
-- their individual accounts. Member accounts don\'t need to authorize the
-- Slack workspace manually through the
-- <https://console.aws.amazon.com/support/app Amazon Web Services Support Center>.
--
-- To use the Amazon Web Services Support App, each account must then
-- complete the following tasks:
--
-- -   Create an Identity and Access Management (IAM) role with the
--     required permission. For more information, see
--     <https://docs.aws.amazon.com/awssupport/latest/user/support-app-permissions.html Managing access to the Amazon Web Services Support App>.
--
-- -   Configure a Slack channel to use the Amazon Web Services Support App
--     for support cases for that account. For more information, see
--     <https://docs.aws.amazon.com/awssupport/latest/user/add-your-slack-channel.html Configuring a Slack channel>.
module Amazonka.SupportApp.RegisterSlackWorkspaceForOrganization
  ( -- * Creating a Request
    RegisterSlackWorkspaceForOrganization (..),
    newRegisterSlackWorkspaceForOrganization,

    -- * Request Lenses
    registerSlackWorkspaceForOrganization_teamId,

    -- * Destructuring the Response
    RegisterSlackWorkspaceForOrganizationResponse (..),
    newRegisterSlackWorkspaceForOrganizationResponse,

    -- * Response Lenses
    registerSlackWorkspaceForOrganizationResponse_accountType,
    registerSlackWorkspaceForOrganizationResponse_teamId,
    registerSlackWorkspaceForOrganizationResponse_teamName,
    registerSlackWorkspaceForOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newRegisterSlackWorkspaceForOrganization' smart constructor.
data RegisterSlackWorkspaceForOrganization = RegisterSlackWorkspaceForOrganization'
  { -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@. Specify the Slack workspace that you want to use
    -- for your organization.
    teamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterSlackWorkspaceForOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'teamId', 'registerSlackWorkspaceForOrganization_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@. Specify the Slack workspace that you want to use
-- for your organization.
newRegisterSlackWorkspaceForOrganization ::
  -- | 'teamId'
  Prelude.Text ->
  RegisterSlackWorkspaceForOrganization
newRegisterSlackWorkspaceForOrganization pTeamId_ =
  RegisterSlackWorkspaceForOrganization'
    { teamId =
        pTeamId_
    }

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@. Specify the Slack workspace that you want to use
-- for your organization.
registerSlackWorkspaceForOrganization_teamId :: Lens.Lens' RegisterSlackWorkspaceForOrganization Prelude.Text
registerSlackWorkspaceForOrganization_teamId = Lens.lens (\RegisterSlackWorkspaceForOrganization' {teamId} -> teamId) (\s@RegisterSlackWorkspaceForOrganization' {} a -> s {teamId = a} :: RegisterSlackWorkspaceForOrganization)

instance
  Core.AWSRequest
    RegisterSlackWorkspaceForOrganization
  where
  type
    AWSResponse
      RegisterSlackWorkspaceForOrganization =
      RegisterSlackWorkspaceForOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterSlackWorkspaceForOrganizationResponse'
            Prelude.<$> (x Data..?> "accountType")
              Prelude.<*> (x Data..?> "teamId")
              Prelude.<*> (x Data..?> "teamName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterSlackWorkspaceForOrganization
  where
  hashWithSalt
    _salt
    RegisterSlackWorkspaceForOrganization' {..} =
      _salt `Prelude.hashWithSalt` teamId

instance
  Prelude.NFData
    RegisterSlackWorkspaceForOrganization
  where
  rnf RegisterSlackWorkspaceForOrganization' {..} =
    Prelude.rnf teamId

instance
  Data.ToHeaders
    RegisterSlackWorkspaceForOrganization
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

instance
  Data.ToJSON
    RegisterSlackWorkspaceForOrganization
  where
  toJSON RegisterSlackWorkspaceForOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("teamId" Data..= teamId)]
      )

instance
  Data.ToPath
    RegisterSlackWorkspaceForOrganization
  where
  toPath =
    Prelude.const
      "/control/register-slack-workspace-for-organization"

instance
  Data.ToQuery
    RegisterSlackWorkspaceForOrganization
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterSlackWorkspaceForOrganizationResponse' smart constructor.
data RegisterSlackWorkspaceForOrganizationResponse = RegisterSlackWorkspaceForOrganizationResponse'
  { -- | Whether the Amazon Web Services account is a management or member
    -- account that\'s part of an organization in Organizations.
    accountType :: Prelude.Maybe AccountType,
    -- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
    -- such as @T012ABCDEFG@.
    teamId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Slack workspace.
    teamName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterSlackWorkspaceForOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountType', 'registerSlackWorkspaceForOrganizationResponse_accountType' - Whether the Amazon Web Services account is a management or member
-- account that\'s part of an organization in Organizations.
--
-- 'teamId', 'registerSlackWorkspaceForOrganizationResponse_teamId' - The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
--
-- 'teamName', 'registerSlackWorkspaceForOrganizationResponse_teamName' - The name of the Slack workspace.
--
-- 'httpStatus', 'registerSlackWorkspaceForOrganizationResponse_httpStatus' - The response's http status code.
newRegisterSlackWorkspaceForOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterSlackWorkspaceForOrganizationResponse
newRegisterSlackWorkspaceForOrganizationResponse
  pHttpStatus_ =
    RegisterSlackWorkspaceForOrganizationResponse'
      { accountType =
          Prelude.Nothing,
        teamId = Prelude.Nothing,
        teamName = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Whether the Amazon Web Services account is a management or member
-- account that\'s part of an organization in Organizations.
registerSlackWorkspaceForOrganizationResponse_accountType :: Lens.Lens' RegisterSlackWorkspaceForOrganizationResponse (Prelude.Maybe AccountType)
registerSlackWorkspaceForOrganizationResponse_accountType = Lens.lens (\RegisterSlackWorkspaceForOrganizationResponse' {accountType} -> accountType) (\s@RegisterSlackWorkspaceForOrganizationResponse' {} a -> s {accountType = a} :: RegisterSlackWorkspaceForOrganizationResponse)

-- | The team ID in Slack. This ID uniquely identifies a Slack workspace,
-- such as @T012ABCDEFG@.
registerSlackWorkspaceForOrganizationResponse_teamId :: Lens.Lens' RegisterSlackWorkspaceForOrganizationResponse (Prelude.Maybe Prelude.Text)
registerSlackWorkspaceForOrganizationResponse_teamId = Lens.lens (\RegisterSlackWorkspaceForOrganizationResponse' {teamId} -> teamId) (\s@RegisterSlackWorkspaceForOrganizationResponse' {} a -> s {teamId = a} :: RegisterSlackWorkspaceForOrganizationResponse)

-- | The name of the Slack workspace.
registerSlackWorkspaceForOrganizationResponse_teamName :: Lens.Lens' RegisterSlackWorkspaceForOrganizationResponse (Prelude.Maybe Prelude.Text)
registerSlackWorkspaceForOrganizationResponse_teamName = Lens.lens (\RegisterSlackWorkspaceForOrganizationResponse' {teamName} -> teamName) (\s@RegisterSlackWorkspaceForOrganizationResponse' {} a -> s {teamName = a} :: RegisterSlackWorkspaceForOrganizationResponse)

-- | The response's http status code.
registerSlackWorkspaceForOrganizationResponse_httpStatus :: Lens.Lens' RegisterSlackWorkspaceForOrganizationResponse Prelude.Int
registerSlackWorkspaceForOrganizationResponse_httpStatus = Lens.lens (\RegisterSlackWorkspaceForOrganizationResponse' {httpStatus} -> httpStatus) (\s@RegisterSlackWorkspaceForOrganizationResponse' {} a -> s {httpStatus = a} :: RegisterSlackWorkspaceForOrganizationResponse)

instance
  Prelude.NFData
    RegisterSlackWorkspaceForOrganizationResponse
  where
  rnf
    RegisterSlackWorkspaceForOrganizationResponse' {..} =
      Prelude.rnf accountType
        `Prelude.seq` Prelude.rnf teamId
        `Prelude.seq` Prelude.rnf teamName
        `Prelude.seq` Prelude.rnf httpStatus
