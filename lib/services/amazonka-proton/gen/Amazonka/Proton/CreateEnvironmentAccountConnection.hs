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
-- Module      : Amazonka.Proton.CreateEnvironmentAccountConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an environment account connection in an environment account so
-- that environment infrastructure resources can be provisioned in the
-- environment account from a management account.
--
-- An environment account connection is a secure bi-directional connection
-- between a /management account/ and an /environment account/ that
-- maintains authorization and permissions. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
module Amazonka.Proton.CreateEnvironmentAccountConnection
  ( -- * Creating a Request
    CreateEnvironmentAccountConnection (..),
    newCreateEnvironmentAccountConnection,

    -- * Request Lenses
    createEnvironmentAccountConnection_clientToken,
    createEnvironmentAccountConnection_codebuildRoleArn,
    createEnvironmentAccountConnection_componentRoleArn,
    createEnvironmentAccountConnection_roleArn,
    createEnvironmentAccountConnection_tags,
    createEnvironmentAccountConnection_environmentName,
    createEnvironmentAccountConnection_managementAccountId,

    -- * Destructuring the Response
    CreateEnvironmentAccountConnectionResponse (..),
    newCreateEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    createEnvironmentAccountConnectionResponse_httpStatus,
    createEnvironmentAccountConnectionResponse_environmentAccountConnection,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironmentAccountConnection' smart constructor.
data CreateEnvironmentAccountConnection = CreateEnvironmentAccountConnection'
  { -- | When included, if two identical requests are made with the same client
    -- token, Proton returns the environment account connection that the first
    -- request created.
    clientToken :: Prelude.Maybe Prelude.Text,
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
    -- You must specify @componentRoleArn@ to allow directly defined components
    -- to be associated with any environments running in this account.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    componentRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM service role that\'s created
    -- in the environment account. Proton uses this role to provision
    -- infrastructure resources in the associated environment account.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | An optional list of metadata items that you can associate with the
    -- Proton environment account connection. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Proton environment that\'s created in the associated
    -- management account.
    environmentName :: Prelude.Text,
    -- | The ID of the management account that accepts or rejects the environment
    -- account connection. You create and manage the Proton environment in this
    -- account. If the management account accepts the environment account
    -- connection, Proton can use the associated IAM role to provision
    -- environment infrastructure resources in the associated environment
    -- account.
    managementAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createEnvironmentAccountConnection_clientToken' - When included, if two identical requests are made with the same client
-- token, Proton returns the environment account connection that the first
-- request created.
--
-- 'codebuildRoleArn', 'createEnvironmentAccountConnection_codebuildRoleArn' - The Amazon Resource Name (ARN) of an IAM service role in the environment
-- account. Proton uses this role to provision infrastructure resources
-- using CodeBuild-based provisioning in the associated environment
-- account.
--
-- 'componentRoleArn', 'createEnvironmentAccountConnection_componentRoleArn' - The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- You must specify @componentRoleArn@ to allow directly defined components
-- to be associated with any environments running in this account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'roleArn', 'createEnvironmentAccountConnection_roleArn' - The Amazon Resource Name (ARN) of the IAM service role that\'s created
-- in the environment account. Proton uses this role to provision
-- infrastructure resources in the associated environment account.
--
-- 'tags', 'createEnvironmentAccountConnection_tags' - An optional list of metadata items that you can associate with the
-- Proton environment account connection. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'environmentName', 'createEnvironmentAccountConnection_environmentName' - The name of the Proton environment that\'s created in the associated
-- management account.
--
-- 'managementAccountId', 'createEnvironmentAccountConnection_managementAccountId' - The ID of the management account that accepts or rejects the environment
-- account connection. You create and manage the Proton environment in this
-- account. If the management account accepts the environment account
-- connection, Proton can use the associated IAM role to provision
-- environment infrastructure resources in the associated environment
-- account.
newCreateEnvironmentAccountConnection ::
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'managementAccountId'
  Prelude.Text ->
  CreateEnvironmentAccountConnection
newCreateEnvironmentAccountConnection
  pEnvironmentName_
  pManagementAccountId_ =
    CreateEnvironmentAccountConnection'
      { clientToken =
          Prelude.Nothing,
        codebuildRoleArn = Prelude.Nothing,
        componentRoleArn = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        environmentName = pEnvironmentName_,
        managementAccountId =
          pManagementAccountId_
      }

-- | When included, if two identical requests are made with the same client
-- token, Proton returns the environment account connection that the first
-- request created.
createEnvironmentAccountConnection_clientToken :: Lens.Lens' CreateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
createEnvironmentAccountConnection_clientToken = Lens.lens (\CreateEnvironmentAccountConnection' {clientToken} -> clientToken) (\s@CreateEnvironmentAccountConnection' {} a -> s {clientToken = a} :: CreateEnvironmentAccountConnection)

-- | The Amazon Resource Name (ARN) of an IAM service role in the environment
-- account. Proton uses this role to provision infrastructure resources
-- using CodeBuild-based provisioning in the associated environment
-- account.
createEnvironmentAccountConnection_codebuildRoleArn :: Lens.Lens' CreateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
createEnvironmentAccountConnection_codebuildRoleArn = Lens.lens (\CreateEnvironmentAccountConnection' {codebuildRoleArn} -> codebuildRoleArn) (\s@CreateEnvironmentAccountConnection' {} a -> s {codebuildRoleArn = a} :: CreateEnvironmentAccountConnection)

-- | The Amazon Resource Name (ARN) of the IAM service role that Proton uses
-- when provisioning directly defined components in the associated
-- environment account. It determines the scope of infrastructure that a
-- component can provision in the account.
--
-- You must specify @componentRoleArn@ to allow directly defined components
-- to be associated with any environments running in this account.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
createEnvironmentAccountConnection_componentRoleArn :: Lens.Lens' CreateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
createEnvironmentAccountConnection_componentRoleArn = Lens.lens (\CreateEnvironmentAccountConnection' {componentRoleArn} -> componentRoleArn) (\s@CreateEnvironmentAccountConnection' {} a -> s {componentRoleArn = a} :: CreateEnvironmentAccountConnection)

-- | The Amazon Resource Name (ARN) of the IAM service role that\'s created
-- in the environment account. Proton uses this role to provision
-- infrastructure resources in the associated environment account.
createEnvironmentAccountConnection_roleArn :: Lens.Lens' CreateEnvironmentAccountConnection (Prelude.Maybe Prelude.Text)
createEnvironmentAccountConnection_roleArn = Lens.lens (\CreateEnvironmentAccountConnection' {roleArn} -> roleArn) (\s@CreateEnvironmentAccountConnection' {} a -> s {roleArn = a} :: CreateEnvironmentAccountConnection)

-- | An optional list of metadata items that you can associate with the
-- Proton environment account connection. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createEnvironmentAccountConnection_tags :: Lens.Lens' CreateEnvironmentAccountConnection (Prelude.Maybe [Tag])
createEnvironmentAccountConnection_tags = Lens.lens (\CreateEnvironmentAccountConnection' {tags} -> tags) (\s@CreateEnvironmentAccountConnection' {} a -> s {tags = a} :: CreateEnvironmentAccountConnection) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Proton environment that\'s created in the associated
-- management account.
createEnvironmentAccountConnection_environmentName :: Lens.Lens' CreateEnvironmentAccountConnection Prelude.Text
createEnvironmentAccountConnection_environmentName = Lens.lens (\CreateEnvironmentAccountConnection' {environmentName} -> environmentName) (\s@CreateEnvironmentAccountConnection' {} a -> s {environmentName = a} :: CreateEnvironmentAccountConnection)

-- | The ID of the management account that accepts or rejects the environment
-- account connection. You create and manage the Proton environment in this
-- account. If the management account accepts the environment account
-- connection, Proton can use the associated IAM role to provision
-- environment infrastructure resources in the associated environment
-- account.
createEnvironmentAccountConnection_managementAccountId :: Lens.Lens' CreateEnvironmentAccountConnection Prelude.Text
createEnvironmentAccountConnection_managementAccountId = Lens.lens (\CreateEnvironmentAccountConnection' {managementAccountId} -> managementAccountId) (\s@CreateEnvironmentAccountConnection' {} a -> s {managementAccountId = a} :: CreateEnvironmentAccountConnection)

instance
  Core.AWSRequest
    CreateEnvironmentAccountConnection
  where
  type
    AWSResponse CreateEnvironmentAccountConnection =
      CreateEnvironmentAccountConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    CreateEnvironmentAccountConnection
  where
  hashWithSalt
    _salt
    CreateEnvironmentAccountConnection' {..} =
      _salt `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` codebuildRoleArn
        `Prelude.hashWithSalt` componentRoleArn
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` environmentName
        `Prelude.hashWithSalt` managementAccountId

instance
  Prelude.NFData
    CreateEnvironmentAccountConnection
  where
  rnf CreateEnvironmentAccountConnection' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf codebuildRoleArn
      `Prelude.seq` Prelude.rnf componentRoleArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf managementAccountId

instance
  Data.ToHeaders
    CreateEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateEnvironmentAccountConnection" ::
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
    CreateEnvironmentAccountConnection
  where
  toJSON CreateEnvironmentAccountConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("codebuildRoleArn" Data..=)
              Prelude.<$> codebuildRoleArn,
            ("componentRoleArn" Data..=)
              Prelude.<$> componentRoleArn,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("environmentName" Data..= environmentName),
            Prelude.Just
              ("managementAccountId" Data..= managementAccountId)
          ]
      )

instance
  Data.ToPath
    CreateEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEnvironmentAccountConnectionResponse' smart constructor.
data CreateEnvironmentAccountConnectionResponse = CreateEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment account connection detail data that\'s returned by
    -- Proton.
    environmentAccountConnection :: EnvironmentAccountConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnection', 'createEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment account connection detail data that\'s returned by
-- Proton.
newCreateEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentAccountConnection'
  EnvironmentAccountConnection ->
  CreateEnvironmentAccountConnectionResponse
newCreateEnvironmentAccountConnectionResponse
  pHttpStatus_
  pEnvironmentAccountConnection_ =
    CreateEnvironmentAccountConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentAccountConnection =
          pEnvironmentAccountConnection_
      }

-- | The response's http status code.
createEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' CreateEnvironmentAccountConnectionResponse Prelude.Int
createEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\CreateEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: CreateEnvironmentAccountConnectionResponse)

-- | The environment account connection detail data that\'s returned by
-- Proton.
createEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' CreateEnvironmentAccountConnectionResponse EnvironmentAccountConnection
createEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\CreateEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@CreateEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: CreateEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    CreateEnvironmentAccountConnectionResponse
  where
  rnf CreateEnvironmentAccountConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnection
