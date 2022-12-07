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
-- Module      : Amazonka.RedshiftServerLess.UpdateNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a namespace with the specified settings.
module Amazonka.RedshiftServerLess.UpdateNamespace
  ( -- * Creating a Request
    UpdateNamespace (..),
    newUpdateNamespace,

    -- * Request Lenses
    updateNamespace_logExports,
    updateNamespace_iamRoles,
    updateNamespace_kmsKeyId,
    updateNamespace_defaultIamRoleArn,
    updateNamespace_adminUserPassword,
    updateNamespace_adminUsername,
    updateNamespace_namespaceName,

    -- * Destructuring the Response
    UpdateNamespaceResponse (..),
    newUpdateNamespaceResponse,

    -- * Response Lenses
    updateNamespaceResponse_httpStatus,
    updateNamespaceResponse_namespace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNamespace' smart constructor.
data UpdateNamespace = UpdateNamespace'
  { -- | The types of logs the namespace can export. The export types are
    -- @userlog@, @connectionlog@, and @useractivitylog@.
    logExports :: Prelude.Maybe [LogExport],
    -- | A list of IAM roles to associate with the namespace.
    iamRoles :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services Key Management Service key used to
    -- encrypt your data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to set as a default in
    -- the namespace.
    defaultIamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The password of the administrator for the first database created in the
    -- namespace.
    adminUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The username of the administrator for the first database created in the
    -- namespace.
    adminUsername :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the namespace.
    namespaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logExports', 'updateNamespace_logExports' - The types of logs the namespace can export. The export types are
-- @userlog@, @connectionlog@, and @useractivitylog@.
--
-- 'iamRoles', 'updateNamespace_iamRoles' - A list of IAM roles to associate with the namespace.
--
-- 'kmsKeyId', 'updateNamespace_kmsKeyId' - The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
--
-- 'defaultIamRoleArn', 'updateNamespace_defaultIamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
--
-- 'adminUserPassword', 'updateNamespace_adminUserPassword' - The password of the administrator for the first database created in the
-- namespace.
--
-- 'adminUsername', 'updateNamespace_adminUsername' - The username of the administrator for the first database created in the
-- namespace.
--
-- 'namespaceName', 'updateNamespace_namespaceName' - The name of the namespace.
newUpdateNamespace ::
  -- | 'namespaceName'
  Prelude.Text ->
  UpdateNamespace
newUpdateNamespace pNamespaceName_ =
  UpdateNamespace'
    { logExports = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      defaultIamRoleArn = Prelude.Nothing,
      adminUserPassword = Prelude.Nothing,
      adminUsername = Prelude.Nothing,
      namespaceName = pNamespaceName_
    }

-- | The types of logs the namespace can export. The export types are
-- @userlog@, @connectionlog@, and @useractivitylog@.
updateNamespace_logExports :: Lens.Lens' UpdateNamespace (Prelude.Maybe [LogExport])
updateNamespace_logExports = Lens.lens (\UpdateNamespace' {logExports} -> logExports) (\s@UpdateNamespace' {} a -> s {logExports = a} :: UpdateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | A list of IAM roles to associate with the namespace.
updateNamespace_iamRoles :: Lens.Lens' UpdateNamespace (Prelude.Maybe [Prelude.Text])
updateNamespace_iamRoles = Lens.lens (\UpdateNamespace' {iamRoles} -> iamRoles) (\s@UpdateNamespace' {} a -> s {iamRoles = a} :: UpdateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
updateNamespace_kmsKeyId :: Lens.Lens' UpdateNamespace (Prelude.Maybe Prelude.Text)
updateNamespace_kmsKeyId = Lens.lens (\UpdateNamespace' {kmsKeyId} -> kmsKeyId) (\s@UpdateNamespace' {} a -> s {kmsKeyId = a} :: UpdateNamespace)

-- | The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
updateNamespace_defaultIamRoleArn :: Lens.Lens' UpdateNamespace (Prelude.Maybe Prelude.Text)
updateNamespace_defaultIamRoleArn = Lens.lens (\UpdateNamespace' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@UpdateNamespace' {} a -> s {defaultIamRoleArn = a} :: UpdateNamespace)

-- | The password of the administrator for the first database created in the
-- namespace.
updateNamespace_adminUserPassword :: Lens.Lens' UpdateNamespace (Prelude.Maybe Prelude.Text)
updateNamespace_adminUserPassword = Lens.lens (\UpdateNamespace' {adminUserPassword} -> adminUserPassword) (\s@UpdateNamespace' {} a -> s {adminUserPassword = a} :: UpdateNamespace) Prelude.. Lens.mapping Data._Sensitive

-- | The username of the administrator for the first database created in the
-- namespace.
updateNamespace_adminUsername :: Lens.Lens' UpdateNamespace (Prelude.Maybe Prelude.Text)
updateNamespace_adminUsername = Lens.lens (\UpdateNamespace' {adminUsername} -> adminUsername) (\s@UpdateNamespace' {} a -> s {adminUsername = a} :: UpdateNamespace) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the namespace.
updateNamespace_namespaceName :: Lens.Lens' UpdateNamespace Prelude.Text
updateNamespace_namespaceName = Lens.lens (\UpdateNamespace' {namespaceName} -> namespaceName) (\s@UpdateNamespace' {} a -> s {namespaceName = a} :: UpdateNamespace)

instance Core.AWSRequest UpdateNamespace where
  type
    AWSResponse UpdateNamespace =
      UpdateNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateNamespaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "namespace")
      )

instance Prelude.Hashable UpdateNamespace where
  hashWithSalt _salt UpdateNamespace' {..} =
    _salt `Prelude.hashWithSalt` logExports
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` adminUserPassword
      `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` namespaceName

instance Prelude.NFData UpdateNamespace where
  rnf UpdateNamespace' {..} =
    Prelude.rnf logExports
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf defaultIamRoleArn
      `Prelude.seq` Prelude.rnf adminUserPassword
      `Prelude.seq` Prelude.rnf adminUsername
      `Prelude.seq` Prelude.rnf namespaceName

instance Data.ToHeaders UpdateNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.UpdateNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNamespace where
  toJSON UpdateNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logExports" Data..=) Prelude.<$> logExports,
            ("iamRoles" Data..=) Prelude.<$> iamRoles,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("defaultIamRoleArn" Data..=)
              Prelude.<$> defaultIamRoleArn,
            ("adminUserPassword" Data..=)
              Prelude.<$> adminUserPassword,
            ("adminUsername" Data..=) Prelude.<$> adminUsername,
            Prelude.Just
              ("namespaceName" Data..= namespaceName)
          ]
      )

instance Data.ToPath UpdateNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNamespaceResponse' smart constructor.
data UpdateNamespaceResponse = UpdateNamespaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of tag instances.
    namespace :: Namespace
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNamespaceResponse_httpStatus' - The response's http status code.
--
-- 'namespace', 'updateNamespaceResponse_namespace' - A list of tag instances.
newUpdateNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'namespace'
  Namespace ->
  UpdateNamespaceResponse
newUpdateNamespaceResponse pHttpStatus_ pNamespace_ =
  UpdateNamespaceResponse'
    { httpStatus = pHttpStatus_,
      namespace = pNamespace_
    }

-- | The response's http status code.
updateNamespaceResponse_httpStatus :: Lens.Lens' UpdateNamespaceResponse Prelude.Int
updateNamespaceResponse_httpStatus = Lens.lens (\UpdateNamespaceResponse' {httpStatus} -> httpStatus) (\s@UpdateNamespaceResponse' {} a -> s {httpStatus = a} :: UpdateNamespaceResponse)

-- | A list of tag instances.
updateNamespaceResponse_namespace :: Lens.Lens' UpdateNamespaceResponse Namespace
updateNamespaceResponse_namespace = Lens.lens (\UpdateNamespaceResponse' {namespace} -> namespace) (\s@UpdateNamespaceResponse' {} a -> s {namespace = a} :: UpdateNamespaceResponse)

instance Prelude.NFData UpdateNamespaceResponse where
  rnf UpdateNamespaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf namespace
