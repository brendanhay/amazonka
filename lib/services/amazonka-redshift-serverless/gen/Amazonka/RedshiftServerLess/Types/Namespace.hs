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
-- Module      : Amazonka.RedshiftServerLess.Types.Namespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.Namespace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types.LogExport
import Amazonka.RedshiftServerLess.Types.NamespaceStatus

-- | A collection of database objects and users.
--
-- /See:/ 'newNamespace' smart constructor.
data Namespace = Namespace'
  { -- | The username of the administrator for the first database created in the
    -- namespace.
    adminUsername :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The date of when the namespace was created.
    creationDate :: Prelude.Maybe Data.ISO8601,
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
    -- User log, Connection log, and User activity log.
    logExports :: Prelude.Maybe [LogExport],
    -- | The Amazon Resource Name (ARN) associated with a namespace.
    namespaceArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of a namespace.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the namespace. Must be between 3-64 alphanumeric characters
    -- in lowercase, and it cannot be a reserved word. A list of reserved words
    -- can be found in
    -- <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    -- in the Amazon Redshift Database Developer Guide.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The status of the namespace.
    status :: Prelude.Maybe NamespaceStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Namespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUsername', 'namespace_adminUsername' - The username of the administrator for the first database created in the
-- namespace.
--
-- 'creationDate', 'namespace_creationDate' - The date of when the namespace was created.
--
-- 'dbName', 'namespace_dbName' - The name of the first database created in the namespace.
--
-- 'defaultIamRoleArn', 'namespace_defaultIamRoleArn' - The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
--
-- 'iamRoles', 'namespace_iamRoles' - A list of IAM roles to associate with the namespace.
--
-- 'kmsKeyId', 'namespace_kmsKeyId' - The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
--
-- 'logExports', 'namespace_logExports' - The types of logs the namespace can export. Available export types are
-- User log, Connection log, and User activity log.
--
-- 'namespaceArn', 'namespace_namespaceArn' - The Amazon Resource Name (ARN) associated with a namespace.
--
-- 'namespaceId', 'namespace_namespaceId' - The unique identifier of a namespace.
--
-- 'namespaceName', 'namespace_namespaceName' - The name of the namespace. Must be between 3-64 alphanumeric characters
-- in lowercase, and it cannot be a reserved word. A list of reserved words
-- can be found in
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
-- in the Amazon Redshift Database Developer Guide.
--
-- 'status', 'namespace_status' - The status of the namespace.
newNamespace ::
  Namespace
newNamespace =
  Namespace'
    { adminUsername = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      dbName = Prelude.Nothing,
      defaultIamRoleArn = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logExports = Prelude.Nothing,
      namespaceArn = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The username of the administrator for the first database created in the
-- namespace.
namespace_adminUsername :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_adminUsername = Lens.lens (\Namespace' {adminUsername} -> adminUsername) (\s@Namespace' {} a -> s {adminUsername = a} :: Namespace) Prelude.. Lens.mapping Data._Sensitive

-- | The date of when the namespace was created.
namespace_creationDate :: Lens.Lens' Namespace (Prelude.Maybe Prelude.UTCTime)
namespace_creationDate = Lens.lens (\Namespace' {creationDate} -> creationDate) (\s@Namespace' {} a -> s {creationDate = a} :: Namespace) Prelude.. Lens.mapping Data._Time

-- | The name of the first database created in the namespace.
namespace_dbName :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_dbName = Lens.lens (\Namespace' {dbName} -> dbName) (\s@Namespace' {} a -> s {dbName = a} :: Namespace)

-- | The Amazon Resource Name (ARN) of the IAM role to set as a default in
-- the namespace.
namespace_defaultIamRoleArn :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_defaultIamRoleArn = Lens.lens (\Namespace' {defaultIamRoleArn} -> defaultIamRoleArn) (\s@Namespace' {} a -> s {defaultIamRoleArn = a} :: Namespace)

-- | A list of IAM roles to associate with the namespace.
namespace_iamRoles :: Lens.Lens' Namespace (Prelude.Maybe [Prelude.Text])
namespace_iamRoles = Lens.lens (\Namespace' {iamRoles} -> iamRoles) (\s@Namespace' {} a -> s {iamRoles = a} :: Namespace) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services Key Management Service key used to
-- encrypt your data.
namespace_kmsKeyId :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_kmsKeyId = Lens.lens (\Namespace' {kmsKeyId} -> kmsKeyId) (\s@Namespace' {} a -> s {kmsKeyId = a} :: Namespace)

-- | The types of logs the namespace can export. Available export types are
-- User log, Connection log, and User activity log.
namespace_logExports :: Lens.Lens' Namespace (Prelude.Maybe [LogExport])
namespace_logExports = Lens.lens (\Namespace' {logExports} -> logExports) (\s@Namespace' {} a -> s {logExports = a} :: Namespace) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) associated with a namespace.
namespace_namespaceArn :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_namespaceArn = Lens.lens (\Namespace' {namespaceArn} -> namespaceArn) (\s@Namespace' {} a -> s {namespaceArn = a} :: Namespace)

-- | The unique identifier of a namespace.
namespace_namespaceId :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_namespaceId = Lens.lens (\Namespace' {namespaceId} -> namespaceId) (\s@Namespace' {} a -> s {namespaceId = a} :: Namespace)

-- | The name of the namespace. Must be between 3-64 alphanumeric characters
-- in lowercase, and it cannot be a reserved word. A list of reserved words
-- can be found in
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
-- in the Amazon Redshift Database Developer Guide.
namespace_namespaceName :: Lens.Lens' Namespace (Prelude.Maybe Prelude.Text)
namespace_namespaceName = Lens.lens (\Namespace' {namespaceName} -> namespaceName) (\s@Namespace' {} a -> s {namespaceName = a} :: Namespace)

-- | The status of the namespace.
namespace_status :: Lens.Lens' Namespace (Prelude.Maybe NamespaceStatus)
namespace_status = Lens.lens (\Namespace' {status} -> status) (\s@Namespace' {} a -> s {status = a} :: Namespace)

instance Data.FromJSON Namespace where
  parseJSON =
    Data.withObject
      "Namespace"
      ( \x ->
          Namespace'
            Prelude.<$> (x Data..:? "adminUsername")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "dbName")
            Prelude.<*> (x Data..:? "defaultIamRoleArn")
            Prelude.<*> (x Data..:? "iamRoles" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "logExports" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namespaceArn")
            Prelude.<*> (x Data..:? "namespaceId")
            Prelude.<*> (x Data..:? "namespaceName")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Namespace where
  hashWithSalt _salt Namespace' {..} =
    _salt `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` defaultIamRoleArn
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logExports
      `Prelude.hashWithSalt` namespaceArn
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` status

instance Prelude.NFData Namespace where
  rnf Namespace' {..} =
    Prelude.rnf adminUsername
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf defaultIamRoleArn
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logExports
      `Prelude.seq` Prelude.rnf namespaceArn
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf status
