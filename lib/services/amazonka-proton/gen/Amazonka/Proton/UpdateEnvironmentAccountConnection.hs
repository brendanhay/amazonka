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
-- Module      : Amazonka.Proton.UpdateEnvironmentAccountConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In an environment account, update an environment account connection to
-- use a new IAM role.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
module Amazonka.Proton.UpdateEnvironmentAccountConnection
  ( -- * Creating a Request
    UpdateEnvironmentAccountConnection (..),
    newUpdateEnvironmentAccountConnection,

    -- * Request Lenses
    updateEnvironmentAccountConnection_roleArn,
    updateEnvironmentAccountConnection_codebuildRoleArn,
    updateEnvironmentAccountConnection_componentRoleArn,
    updateEnvironmentAccountConnection_id,

    -- * Destructuring the Response
    UpdateEnvironmentAccountConnectionResponse (..),
    newUpdateEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    updateEnvironmentAccountConnectionResponse_httpStatus,
    updateEnvironmentAccountConnectionResponse_environmentAccountConnection,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironmentAccountConnection' smart constructor.
data UpdateEnvironmentAccountConnection = UpdateEnvironmentAccountConnection'
  { -- | The Amazon Resource Name (ARN) of the IAM service role that\'s
    -- associated with the environment account connection to update.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an IAM service role in the environment
    -- account. Proton uses this role to provision infrastructure resources
    -- using CodeBuild-based provisioning in the associated environment
    -- account.
    codebuildRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
    -- when provisioning directly defined components in the associated
    -- environment account. It determines the scope of infrastructure that a
    -- component can provision in the account.
    --
    -- The environment account connection must have a @componentRoleArn@ to
    -- allow directly defined components to be associated with any environments
    -- running in the account.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    componentRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateEnvironmentAccountConnection_roleArn' - The Amazon Resource Name (ARN) of the IAM service role that\'s
-- associated with the environment account connection to update.
--
-- 'codebuildRoleArn', 'updateEnvironmentAccountConnection_codebuildRoleArn' - The Amazon Resource Name (ARN) of an IAM service role in the environment
-- account. Proton uses this role to provision infrastructure resources
-- using CodeBuild-based provisioning in the associated environment
-- account.
--
-- 'componentRoleArn', 'updateEnvironmentAccountConnection_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- The environment account connection must have a @componentRoleArn@ to
-- allow directly defined components to be associated with any environments
-- running in the account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'id', 'updateEnvironmentAccountConnection_id' - The ID of the environment account connection to update.
newUpdateEnvironmentAccountConnection ::
  -- | 'id'
  Prelude.Text ->
  UpdateEnvironmentAccountConnection
newUpdateEnvironmentAccountConnection pId_ =
  UpdateEnvironmentAccountConnection'
    { roleArn =
        Prelude.Nothing,
      codebuildRoleArn = Prelude.Nothing,
      componentRoleArn = Prelude.Nothing,
      id = pId_
    }

-- | The Amazon Resource Name (ARN) of the IAM service role that\'s
-- associated with the environment account connection to update.
updateEnvironmentAccountConnection_roleArn :: Lens.Lens' UpdateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
updateEnvironmentAccountConnection_roleArn = Lens.lens (\UpdateEnvironmentAccountConnection' {roleArn} -> roleArn) (\s@UpdateEnvironmentAccountConnection' {} a -> s {roleArn = a} :: UpdateEnvironmentAccountConnection)

-- | The Amazon Resource Name (ARN) of an IAM service role in the environment
-- account. Proton uses this role to provision infrastructure resources
-- using CodeBuild-based provisioning in the associated environment
-- account.
updateEnvironmentAccountConnection_codebuildRoleArn :: Lens.Lens' UpdateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
updateEnvironmentAccountConnection_codebuildRoleArn = Lens.lens (\UpdateEnvironmentAccountConnection' {codebuildRoleArn} -> codebuildRoleArn) (\s@UpdateEnvironmentAccountConnection' {} a -> s {codebuildRoleArn = a} :: UpdateEnvironmentAccountConnection)

-- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- The environment account connection must have a @componentRoleArn@ to
-- allow directly defined components to be associated with any environments
-- running in the account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
updateEnvironmentAccountConnection_componentRoleArn :: Lens.Lens' UpdateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
updateEnvironmentAccountConnection_componentRoleArn = Lens.lens (\UpdateEnvironmentAccountConnection' {componentRoleArn} -> componentRoleArn) (\s@UpdateEnvironmentAccountConnection' {} a -> s {componentRoleArn = a} :: UpdateEnvironmentAccountConnection)

-- | The ID of the environment account connection to update.
updateEnvironmentAccountConnection_id :: Lens.Lens' UpdateEnvironmentAccountConnection Prelude.Text
updateEnvironmentAccountConnection_id = Lens.lens (\UpdateEnvironmentAccountConnection' {id} -> id) (\s@UpdateEnvironmentAccountConnection' {} a -> s {id = a} :: UpdateEnvironmentAccountConnection)

instance
  Core.AWSRequest
    UpdateEnvironmentAccountConnection
  where
  type
    AWSResponse UpdateEnvironmentAccountConnection =
      UpdateEnvironmentAccountConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    UpdateEnvironmentAccountConnection
  where
  hashWithSalt
    _salt
    UpdateEnvironmentAccountConnection' {..} =
      _salt `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` codebuildRoleArn
        `Prelude.hashWithSalt` componentRoleArn
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateEnvironmentAccountConnection
  where
  rnf UpdateEnvironmentAccountConnection' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf codebuildRoleArn
      `Prelude.seq` Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateEnvironmentAccountConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateEnvironmentAccountConnection
  where
  toJSON UpdateEnvironmentAccountConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("roleArn" Data..=) Prelude.<$> roleArn,
            ("codebuildRoleArn" Data..=)
              Prelude.<$> codebuildRoleArn,
            ("componentRoleArn" Data..=)
              Prelude.<$> componentRoleArn,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance
  Data.ToPath
    UpdateEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentAccountConnectionResponse' smart constructor.
data UpdateEnvironmentAccountConnectionResponse = UpdateEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment account connection detail data that\'s returned by
    -- Proton.
    environmentAccountConnection :: EnvironmentAccountConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnection', 'updateEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment account connection detail data that\'s returned by
-- Proton.
newUpdateEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentAccountConnection'
  EnvironmentAccountConnection ->
  UpdateEnvironmentAccountConnectionResponse
newUpdateEnvironmentAccountConnectionResponse
  pHttpStatus_
  pEnvironmentAccountConnection_ =
    UpdateEnvironmentAccountConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentAccountConnection =
          pEnvironmentAccountConnection_
      }

-- | The response's http status code.
updateEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' UpdateEnvironmentAccountConnectionResponse Prelude.Int
updateEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\UpdateEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentAccountConnectionResponse)

-- | The environment account connection detail data that\'s returned by
-- Proton.
updateEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' UpdateEnvironmentAccountConnectionResponse EnvironmentAccountConnection
updateEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\UpdateEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@UpdateEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: UpdateEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    UpdateEnvironmentAccountConnectionResponse
  where
  rnf UpdateEnvironmentAccountConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnection
