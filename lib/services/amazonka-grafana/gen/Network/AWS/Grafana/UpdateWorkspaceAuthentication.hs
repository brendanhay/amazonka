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
-- Module      : Network.AWS.Grafana.UpdateWorkspaceAuthentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to define the identity provider (IdP) that this
-- workspace authenticates users from, using SAML. You can also map SAML
-- assertion attributes to workspace user information and define which
-- groups in the assertion attribute are to have the @Admin@ and @Editor@
-- roles in the workspace.
module Network.AWS.Grafana.UpdateWorkspaceAuthentication
  ( -- * Creating a Request
    UpdateWorkspaceAuthentication (..),
    newUpdateWorkspaceAuthentication,

    -- * Request Lenses
    updateWorkspaceAuthentication_samlConfiguration,
    updateWorkspaceAuthentication_authenticationProviders,
    updateWorkspaceAuthentication_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceAuthenticationResponse (..),
    newUpdateWorkspaceAuthenticationResponse,

    -- * Response Lenses
    updateWorkspaceAuthenticationResponse_httpStatus,
    updateWorkspaceAuthenticationResponse_authentication,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateWorkspaceAuthentication' smart constructor.
data UpdateWorkspaceAuthentication = UpdateWorkspaceAuthentication'
  { -- | If the workspace uses SAML, use this structure to map SAML assertion
    -- attributes to workspace user information and define which groups in the
    -- assertion attribute are to have the @Admin@ and @Editor@ roles in the
    -- workspace.
    samlConfiguration :: Prelude.Maybe SamlConfiguration,
    -- | Specifies whether this workspace uses SAML 2.0, Amazon Web Services
    -- Single Sign On, or both to authenticate users for using the Grafana
    -- console within a workspace. For more information, see
    -- <https://docs.aws.amazon.com/grafana/latest/userguide/authentication-in-AMG.html User authentication in Amazon Managed Grafana>.
    authenticationProviders :: [AuthenticationProviderTypes],
    -- | The ID of the workspace to update the authentication for.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'samlConfiguration', 'updateWorkspaceAuthentication_samlConfiguration' - If the workspace uses SAML, use this structure to map SAML assertion
-- attributes to workspace user information and define which groups in the
-- assertion attribute are to have the @Admin@ and @Editor@ roles in the
-- workspace.
--
-- 'authenticationProviders', 'updateWorkspaceAuthentication_authenticationProviders' - Specifies whether this workspace uses SAML 2.0, Amazon Web Services
-- Single Sign On, or both to authenticate users for using the Grafana
-- console within a workspace. For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/authentication-in-AMG.html User authentication in Amazon Managed Grafana>.
--
-- 'workspaceId', 'updateWorkspaceAuthentication_workspaceId' - The ID of the workspace to update the authentication for.
newUpdateWorkspaceAuthentication ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspaceAuthentication
newUpdateWorkspaceAuthentication pWorkspaceId_ =
  UpdateWorkspaceAuthentication'
    { samlConfiguration =
        Prelude.Nothing,
      authenticationProviders = Prelude.mempty,
      workspaceId = pWorkspaceId_
    }

-- | If the workspace uses SAML, use this structure to map SAML assertion
-- attributes to workspace user information and define which groups in the
-- assertion attribute are to have the @Admin@ and @Editor@ roles in the
-- workspace.
updateWorkspaceAuthentication_samlConfiguration :: Lens.Lens' UpdateWorkspaceAuthentication (Prelude.Maybe SamlConfiguration)
updateWorkspaceAuthentication_samlConfiguration = Lens.lens (\UpdateWorkspaceAuthentication' {samlConfiguration} -> samlConfiguration) (\s@UpdateWorkspaceAuthentication' {} a -> s {samlConfiguration = a} :: UpdateWorkspaceAuthentication)

-- | Specifies whether this workspace uses SAML 2.0, Amazon Web Services
-- Single Sign On, or both to authenticate users for using the Grafana
-- console within a workspace. For more information, see
-- <https://docs.aws.amazon.com/grafana/latest/userguide/authentication-in-AMG.html User authentication in Amazon Managed Grafana>.
updateWorkspaceAuthentication_authenticationProviders :: Lens.Lens' UpdateWorkspaceAuthentication [AuthenticationProviderTypes]
updateWorkspaceAuthentication_authenticationProviders = Lens.lens (\UpdateWorkspaceAuthentication' {authenticationProviders} -> authenticationProviders) (\s@UpdateWorkspaceAuthentication' {} a -> s {authenticationProviders = a} :: UpdateWorkspaceAuthentication) Prelude.. Lens.coerced

-- | The ID of the workspace to update the authentication for.
updateWorkspaceAuthentication_workspaceId :: Lens.Lens' UpdateWorkspaceAuthentication Prelude.Text
updateWorkspaceAuthentication_workspaceId = Lens.lens (\UpdateWorkspaceAuthentication' {workspaceId} -> workspaceId) (\s@UpdateWorkspaceAuthentication' {} a -> s {workspaceId = a} :: UpdateWorkspaceAuthentication)

instance
  Core.AWSRequest
    UpdateWorkspaceAuthentication
  where
  type
    AWSResponse UpdateWorkspaceAuthentication =
      UpdateWorkspaceAuthenticationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkspaceAuthenticationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "authentication")
      )

instance
  Prelude.Hashable
    UpdateWorkspaceAuthentication

instance Prelude.NFData UpdateWorkspaceAuthentication

instance Core.ToHeaders UpdateWorkspaceAuthentication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorkspaceAuthentication where
  toJSON UpdateWorkspaceAuthentication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("samlConfiguration" Core..=)
              Prelude.<$> samlConfiguration,
            Prelude.Just
              ( "authenticationProviders"
                  Core..= authenticationProviders
              )
          ]
      )

instance Core.ToPath UpdateWorkspaceAuthentication where
  toPath UpdateWorkspaceAuthentication' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/authentication"
      ]

instance Core.ToQuery UpdateWorkspaceAuthentication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceAuthenticationResponse' smart constructor.
data UpdateWorkspaceAuthenticationResponse = UpdateWorkspaceAuthenticationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that describes the user authentication for this workspace
    -- after the update is made.
    authentication :: AuthenticationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceAuthenticationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceAuthenticationResponse_httpStatus' - The response's http status code.
--
-- 'authentication', 'updateWorkspaceAuthenticationResponse_authentication' - A structure that describes the user authentication for this workspace
-- after the update is made.
newUpdateWorkspaceAuthenticationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'authentication'
  AuthenticationDescription ->
  UpdateWorkspaceAuthenticationResponse
newUpdateWorkspaceAuthenticationResponse
  pHttpStatus_
  pAuthentication_ =
    UpdateWorkspaceAuthenticationResponse'
      { httpStatus =
          pHttpStatus_,
        authentication = pAuthentication_
      }

-- | The response's http status code.
updateWorkspaceAuthenticationResponse_httpStatus :: Lens.Lens' UpdateWorkspaceAuthenticationResponse Prelude.Int
updateWorkspaceAuthenticationResponse_httpStatus = Lens.lens (\UpdateWorkspaceAuthenticationResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceAuthenticationResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceAuthenticationResponse)

-- | A structure that describes the user authentication for this workspace
-- after the update is made.
updateWorkspaceAuthenticationResponse_authentication :: Lens.Lens' UpdateWorkspaceAuthenticationResponse AuthenticationDescription
updateWorkspaceAuthenticationResponse_authentication = Lens.lens (\UpdateWorkspaceAuthenticationResponse' {authentication} -> authentication) (\s@UpdateWorkspaceAuthenticationResponse' {} a -> s {authentication = a} :: UpdateWorkspaceAuthenticationResponse)

instance
  Prelude.NFData
    UpdateWorkspaceAuthenticationResponse
