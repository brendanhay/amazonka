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
-- Module      : Amazonka.RedshiftServerLess.CreateNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a namespace in Amazon Redshift Serverless.
module Amazonka.RedshiftServerLess.CreateNamespace
  ( -- * Creating a Request
    CreateNamespace (..),
    newCreateNamespace,

    -- * Request Lenses
    createNamespace_adminUserPassword,
    createNamespace_adminUsername,
    createNamespace_dbName,
    createNamespace_defaultIamRoleArn,
    createNamespace_iamRoles,
    createNamespace_kmsKeyId,
    createNamespace_logExports,
    createNamespace_tags,
    createNamespace_namespaceName,

    -- * Destructuring the Response
    CreateNamespaceResponse (..),
    newCreateNamespaceResponse,

    -- * Response Lenses
    createNamespaceResponse_namespace,
    createNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNamespace' smart constructor.
data CreateNamespace = CreateNamespace'
  { -- | The password of the administrator for the first database created in the
    -- namespace.
    adminUserPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The username of the administrator for the first database created in the
    -- namespace.
    adminUsername :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the first database created in the namespace.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to set as a default in
    -- the namespace.
    defaultIamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of IAM roles to associate with the namespace.
    iamRoles :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the Amazon Web Services Key Management Service key used to
    -- encrypt your data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The types of logs the namespace can export. Available export types are
    -- @userlog@, @connectionlog@, and @useractivitylog@.
    logExports :: Prelude.Maybe [LogExport],
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the namespace.
    namespaceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUserPassword', 'createNamespace_adminUserPassword' - The password of the administrator for the first database created in the
-- namespace.
--
-- 'adminUsername', 'createNamespace_adminUsername' - The username of the administrator for the first database created in the
-- namespace.
--
-- 'dbName', 'createNamespace_dbName' - The name of the first database created in the namespace.
--
-- 'defaultIamRoleArn', 'createNamespace_defaultIamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
--
-- 'iamRoles', 'createNamespace_iamRoles' - A list of IAM roles to associate with the namespace.
--
-- 'kmsKeyId', 'createNamespace_kmsKeyId' - The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
--
-- 'logExports', 'createNamespace_logExports' - The types of logs the namespace can export. Available export types are
-- @userlog@, @connectionlog@, and @useractivitylog@.
--
-- 'tags', 'createNamespace_tags' - A list of tag instances.
--
-- 'namespaceName', 'createNamespace_namespaceName' - The name of the namespace.
newCreateNamespace ::
  -- | 'namespaceName'
  Prelude.Text ->
  CreateNamespace
newCreateNamespace pNamespaceName_ =
  CreateNamespace'
    { adminUserPassword =
        Prelude.Nothing,
      adminUsername = Prelude.Nothing,
      dbName = Prelude.Nothing,
      defaultIamRoleArn = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logExports = Prelude.Nothing,
      tags = Prelude.Nothing,
      namespaceName = pNamespaceName_
    }

-- | The password of the administrator for the first database created in the
-- namespace.
createNamespace_adminUserPassword :: Lens.Lens' CreateNamespace (Prelude.Maybe Prelude.Text)
createNamespace_adminUserPassword = Lens.lens (\CreateNamespace' {adminUserPassword} -> adminUserPassword) (\s@CreateNamespace' {} a -> s {adminUserPassword = a} :: CreateNamespace) Prelude.. Lens.mapping Data._Sensitive

-- | The username of the administrator for the first database created in the
-- namespace.
createNamespace_adminUsername :: Lens.Lens' CreateNamespace (Prelude.Maybe Prelude.Text)
createNamespace_adminUsername = Lens.lens (\CreateNamespace' {adminUsername} -> adminUsername) (\s@CreateNamespace' {} a -> s {adminUsername = a} :: CreateNamespace) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the first database created in the namespace.
createNamespace_dbName :: Lens.Lens' CreateNamespace (Prelude.Maybe Prelude.Text)
createNamespace_dbName = Lens.lens (\CreateNamespace' {dbName} -> dbName) (\s@CreateNamespace' {} a -> s {dbName = a} :: CreateNamespace)

-- | The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
createNamespace_defaultIamRoleArn :: Lens.Lens' CreateNamespace (Prelude.Maybe Prelude.Text)
createNamespace_defaultIamRoleArn = Lens.lens (\CreateNamespace' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@CreateNamespace' {} a -> s {defaultIamRoleArn = a} :: CreateNamespace)

-- | A list of IAM roles to associate with the namespace.
createNamespace_iamRoles :: Lens.Lens' CreateNamespace (Prelude.Maybe [Prelude.Text])
createNamespace_iamRoles = Lens.lens (\CreateNamespace' {iamRoles} -> iamRoles) (\s@CreateNamespace' {} a -> s {iamRoles = a} :: CreateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
createNamespace_kmsKeyId :: Lens.Lens' CreateNamespace (Prelude.Maybe Prelude.Text)
createNamespace_kmsKeyId = Lens.lens (\CreateNamespace' {kmsKeyId} -> kmsKeyId) (\s@CreateNamespace' {} a -> s {kmsKeyId = a} :: CreateNamespace)

-- | The types of logs the namespace can export. Available export types are
-- @userlog@, @connectionlog@, and @useractivitylog@.
createNamespace_logExports :: Lens.Lens' CreateNamespace (Prelude.Maybe [LogExport])
createNamespace_logExports = Lens.lens (\CreateNamespace' {logExports} -> logExports) (\s@CreateNamespace' {} a -> s {logExports = a} :: CreateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | A list of tag instances.
createNamespace_tags :: Lens.Lens' CreateNamespace (Prelude.Maybe [Tag])
createNamespace_tags = Lens.lens (\CreateNamespace' {tags} -> tags) (\s@CreateNamespace' {} a -> s {tags = a} :: CreateNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The name of the namespace.
createNamespace_namespaceName :: Lens.Lens' CreateNamespace Prelude.Text
createNamespace_namespaceName = Lens.lens (\CreateNamespace' {namespaceName} -> namespaceName) (\s@CreateNamespace' {} a -> s {namespaceName = a} :: CreateNamespace)

instance Core.AWSRequest CreateNamespace where
  type
    AWSResponse CreateNamespace =
      CreateNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNamespaceResponse'
            Prelude.<$> (x Data..?> "namespace")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNamespace where
  hashWithSalt _salt CreateNamespace' {..} =
    _salt `Prelude.hashWithSalt` adminUserPassword
      `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logExports
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` namespaceName

instance Prelude.NFData CreateNamespace where
  rnf CreateNamespace' {..} =
    Prelude.rnf adminUserPassword
      `Prelude.seq` Prelude.rnf adminUsername
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf defaultIamRoleArn
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logExports
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf namespaceName

instance Data.ToHeaders CreateNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.CreateNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateNamespace where
  toJSON CreateNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adminUserPassword" Data..=)
              Prelude.<$> adminUserPassword,
            ("adminUsername" Data..=) Prelude.<$> adminUsername,
            ("dbName" Data..=) Prelude.<$> dbName,
            ("defaultIamRoleArn" Data..=)
              Prelude.<$> defaultIamRoleArn,
            ("iamRoles" Data..=) Prelude.<$> iamRoles,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("logExports" Data..=) Prelude.<$> logExports,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("namespaceName" Data..= namespaceName)
          ]
      )

instance Data.ToPath CreateNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNamespaceResponse' smart constructor.
data CreateNamespaceResponse = CreateNamespaceResponse'
  { -- | The created namespace object.
    namespace :: Prelude.Maybe Namespace,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespace', 'createNamespaceResponse_namespace' - The created namespace object.
--
-- 'httpStatus', 'createNamespaceResponse_httpStatus' - The response's http status code.
newCreateNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNamespaceResponse
newCreateNamespaceResponse pHttpStatus_ =
  CreateNamespaceResponse'
    { namespace =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The created namespace object.
createNamespaceResponse_namespace :: Lens.Lens' CreateNamespaceResponse (Prelude.Maybe Namespace)
createNamespaceResponse_namespace = Lens.lens (\CreateNamespaceResponse' {namespace} -> namespace) (\s@CreateNamespaceResponse' {} a -> s {namespace = a} :: CreateNamespaceResponse)

-- | The response's http status code.
createNamespaceResponse_httpStatus :: Lens.Lens' CreateNamespaceResponse Prelude.Int
createNamespaceResponse_httpStatus = Lens.lens (\CreateNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreateNamespaceResponse' {} a -> s {httpStatus = a} :: CreateNamespaceResponse)

instance Prelude.NFData CreateNamespaceResponse where
  rnf CreateNamespaceResponse' {..} =
    Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf httpStatus
