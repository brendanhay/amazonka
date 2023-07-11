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
-- Module      : Amazonka.Lightsail.CreateRelationalDatabaseFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new database from an existing database snapshot in Amazon
-- Lightsail.
--
-- You can create a new database from a snapshot in if something goes wrong
-- with your original database, or to change it to a different plan, such
-- as a high availability or standard plan.
--
-- The @create relational database from snapshot@ operation supports
-- tag-based access control via request tags and resource tags applied to
-- the resource identified by relationalDatabaseSnapshotName. For more
-- information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateRelationalDatabaseFromSnapshot
  ( -- * Creating a Request
    CreateRelationalDatabaseFromSnapshot (..),
    newCreateRelationalDatabaseFromSnapshot,

    -- * Request Lenses
    createRelationalDatabaseFromSnapshot_availabilityZone,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_tags,
    createRelationalDatabaseFromSnapshot_useLatestRestorableTime,
    createRelationalDatabaseFromSnapshot_relationalDatabaseName,

    -- * Destructuring the Response
    CreateRelationalDatabaseFromSnapshotResponse (..),
    newCreateRelationalDatabaseFromSnapshotResponse,

    -- * Response Lenses
    createRelationalDatabaseFromSnapshotResponse_operations,
    createRelationalDatabaseFromSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRelationalDatabaseFromSnapshot' smart constructor.
data CreateRelationalDatabaseFromSnapshot = CreateRelationalDatabaseFromSnapshot'
  { -- | The Availability Zone in which to create your new database. Use the
    -- @us-east-2a@ case-sensitive format.
    --
    -- You can get a list of Availability Zones by using the @get regions@
    -- operation. Be sure to add the
    -- @include relational database Availability Zones@ parameter to your
    -- request.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the accessibility options for your new database. A value of
    -- @true@ specifies a database that is available to resources outside of
    -- your Lightsail account. A value of @false@ specifies a database that is
    -- available only to your Lightsail resources in the same region as your
    -- database.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The bundle ID for your new database. A bundle describes the performance
    -- specifications for your database.
    --
    -- You can get a list of database bundle IDs by using the
    -- @get relational database bundles@ operation.
    --
    -- When creating a new database from a snapshot, you cannot choose a bundle
    -- that is smaller than the bundle of the source database.
    relationalDatabaseBundleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database snapshot from which to create your new
    -- database.
    relationalDatabaseSnapshotName :: Prelude.Maybe Prelude.Text,
    -- | The date and time to restore your database from.
    --
    -- Constraints:
    --
    -- -   Must be before the latest restorable time for the database.
    --
    -- -   Cannot be specified if the @use latest restorable time@ parameter is
    --     @true@.
    --
    -- -   Specified in Coordinated Universal Time (UTC).
    --
    -- -   Specified in the Unix time format.
    --
    --     For example, if you wish to use a restore time of October 1, 2018,
    --     at 8 PM UTC, then you input @1538424000@ as the restore time.
    restoreTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the source database.
    sourceRelationalDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies whether your database is restored from the latest backup time.
    -- A value of @true@ restores from the latest backup time.
    --
    -- Default: @false@
    --
    -- Constraints: Cannot be specified if the @restore time@ parameter is
    -- provided.
    useLatestRestorableTime :: Prelude.Maybe Prelude.Bool,
    -- | The name to use for your new Lightsail database resource.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'createRelationalDatabaseFromSnapshot_availabilityZone' - The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
--
-- 'publiclyAccessible', 'createRelationalDatabaseFromSnapshot_publiclyAccessible' - Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
--
-- 'relationalDatabaseBundleId', 'createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId' - The bundle ID for your new database. A bundle describes the performance
-- specifications for your database.
--
-- You can get a list of database bundle IDs by using the
-- @get relational database bundles@ operation.
--
-- When creating a new database from a snapshot, you cannot choose a bundle
-- that is smaller than the bundle of the source database.
--
-- 'relationalDatabaseSnapshotName', 'createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName' - The name of the database snapshot from which to create your new
-- database.
--
-- 'restoreTime', 'createRelationalDatabaseFromSnapshot_restoreTime' - The date and time to restore your database from.
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the database.
--
-- -   Cannot be specified if the @use latest restorable time@ parameter is
--     @true@.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a restore time of October 1, 2018,
--     at 8 PM UTC, then you input @1538424000@ as the restore time.
--
-- 'sourceRelationalDatabaseName', 'createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName' - The name of the source database.
--
-- 'tags', 'createRelationalDatabaseFromSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'useLatestRestorableTime', 'createRelationalDatabaseFromSnapshot_useLatestRestorableTime' - Specifies whether your database is restored from the latest backup time.
-- A value of @true@ restores from the latest backup time.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if the @restore time@ parameter is
-- provided.
--
-- 'relationalDatabaseName', 'createRelationalDatabaseFromSnapshot_relationalDatabaseName' - The name to use for your new Lightsail database resource.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
newCreateRelationalDatabaseFromSnapshot ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  CreateRelationalDatabaseFromSnapshot
newCreateRelationalDatabaseFromSnapshot
  pRelationalDatabaseName_ =
    CreateRelationalDatabaseFromSnapshot'
      { availabilityZone =
          Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        relationalDatabaseBundleId =
          Prelude.Nothing,
        relationalDatabaseSnapshotName =
          Prelude.Nothing,
        restoreTime = Prelude.Nothing,
        sourceRelationalDatabaseName =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        useLatestRestorableTime =
          Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
createRelationalDatabaseFromSnapshot_availabilityZone :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Text)
createRelationalDatabaseFromSnapshot_availabilityZone = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {availabilityZone = a} :: CreateRelationalDatabaseFromSnapshot)

-- | Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
createRelationalDatabaseFromSnapshot_publiclyAccessible :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Bool)
createRelationalDatabaseFromSnapshot_publiclyAccessible = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {publiclyAccessible = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The bundle ID for your new database. A bundle describes the performance
-- specifications for your database.
--
-- You can get a list of database bundle IDs by using the
-- @get relational database bundles@ operation.
--
-- When creating a new database from a snapshot, you cannot choose a bundle
-- that is smaller than the bundle of the source database.
createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Text)
createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseBundleId} -> relationalDatabaseBundleId) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseBundleId = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The name of the database snapshot from which to create your new
-- database.
createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Text)
createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The date and time to restore your database from.
--
-- Constraints:
--
-- -   Must be before the latest restorable time for the database.
--
-- -   Cannot be specified if the @use latest restorable time@ parameter is
--     @true@.
--
-- -   Specified in Coordinated Universal Time (UTC).
--
-- -   Specified in the Unix time format.
--
--     For example, if you wish to use a restore time of October 1, 2018,
--     at 8 PM UTC, then you input @1538424000@ as the restore time.
createRelationalDatabaseFromSnapshot_restoreTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.UTCTime)
createRelationalDatabaseFromSnapshot_restoreTime = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {restoreTime} -> restoreTime) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {restoreTime = a} :: CreateRelationalDatabaseFromSnapshot) Prelude.. Lens.mapping Data._Time

-- | The name of the source database.
createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Text)
createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {sourceRelationalDatabaseName} -> sourceRelationalDatabaseName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {sourceRelationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createRelationalDatabaseFromSnapshot_tags :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe [Tag])
createRelationalDatabaseFromSnapshot_tags = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {tags} -> tags) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {tags = a} :: CreateRelationalDatabaseFromSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether your database is restored from the latest backup time.
-- A value of @true@ restores from the latest backup time.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if the @restore time@ parameter is
-- provided.
createRelationalDatabaseFromSnapshot_useLatestRestorableTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Prelude.Maybe Prelude.Bool)
createRelationalDatabaseFromSnapshot_useLatestRestorableTime = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {useLatestRestorableTime = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The name to use for your new Lightsail database resource.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
createRelationalDatabaseFromSnapshot_relationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot Prelude.Text
createRelationalDatabaseFromSnapshot_relationalDatabaseName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseName} -> relationalDatabaseName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)

instance
  Core.AWSRequest
    CreateRelationalDatabaseFromSnapshot
  where
  type
    AWSResponse CreateRelationalDatabaseFromSnapshot =
      CreateRelationalDatabaseFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseFromSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateRelationalDatabaseFromSnapshot
  where
  hashWithSalt
    _salt
    CreateRelationalDatabaseFromSnapshot' {..} =
      _salt
        `Prelude.hashWithSalt` availabilityZone
        `Prelude.hashWithSalt` publiclyAccessible
        `Prelude.hashWithSalt` relationalDatabaseBundleId
        `Prelude.hashWithSalt` relationalDatabaseSnapshotName
        `Prelude.hashWithSalt` restoreTime
        `Prelude.hashWithSalt` sourceRelationalDatabaseName
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` useLatestRestorableTime
        `Prelude.hashWithSalt` relationalDatabaseName

instance
  Prelude.NFData
    CreateRelationalDatabaseFromSnapshot
  where
  rnf CreateRelationalDatabaseFromSnapshot' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf relationalDatabaseBundleId
      `Prelude.seq` Prelude.rnf relationalDatabaseSnapshotName
      `Prelude.seq` Prelude.rnf restoreTime
      `Prelude.seq` Prelude.rnf sourceRelationalDatabaseName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf useLatestRestorableTime
      `Prelude.seq` Prelude.rnf relationalDatabaseName

instance
  Data.ToHeaders
    CreateRelationalDatabaseFromSnapshot
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateRelationalDatabaseFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateRelationalDatabaseFromSnapshot
  where
  toJSON CreateRelationalDatabaseFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("availabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("publiclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("relationalDatabaseBundleId" Data..=)
              Prelude.<$> relationalDatabaseBundleId,
            ("relationalDatabaseSnapshotName" Data..=)
              Prelude.<$> relationalDatabaseSnapshotName,
            ("restoreTime" Data..=) Prelude.<$> restoreTime,
            ("sourceRelationalDatabaseName" Data..=)
              Prelude.<$> sourceRelationalDatabaseName,
            ("tags" Data..=) Prelude.<$> tags,
            ("useLatestRestorableTime" Data..=)
              Prelude.<$> useLatestRestorableTime,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance
  Data.ToPath
    CreateRelationalDatabaseFromSnapshot
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateRelationalDatabaseFromSnapshot
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRelationalDatabaseFromSnapshotResponse' smart constructor.
data CreateRelationalDatabaseFromSnapshotResponse = CreateRelationalDatabaseFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createRelationalDatabaseFromSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createRelationalDatabaseFromSnapshotResponse_httpStatus' - The response's http status code.
newCreateRelationalDatabaseFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRelationalDatabaseFromSnapshotResponse
newCreateRelationalDatabaseFromSnapshotResponse
  pHttpStatus_ =
    CreateRelationalDatabaseFromSnapshotResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createRelationalDatabaseFromSnapshotResponse_operations :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse (Prelude.Maybe [Operation])
createRelationalDatabaseFromSnapshotResponse_operations = Lens.lens (\CreateRelationalDatabaseFromSnapshotResponse' {operations} -> operations) (\s@CreateRelationalDatabaseFromSnapshotResponse' {} a -> s {operations = a} :: CreateRelationalDatabaseFromSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createRelationalDatabaseFromSnapshotResponse_httpStatus :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse Prelude.Int
createRelationalDatabaseFromSnapshotResponse_httpStatus = Lens.lens (\CreateRelationalDatabaseFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateRelationalDatabaseFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateRelationalDatabaseFromSnapshotResponse)

instance
  Prelude.NFData
    CreateRelationalDatabaseFromSnapshotResponse
  where
  rnf CreateRelationalDatabaseFromSnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
