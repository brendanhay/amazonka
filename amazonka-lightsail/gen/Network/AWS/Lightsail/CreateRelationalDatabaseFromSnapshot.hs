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
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
  ( -- * Creating a Request
    CreateRelationalDatabaseFromSnapshot (..),
    newCreateRelationalDatabaseFromSnapshot,

    -- * Request Lenses
    createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId,
    createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName,
    createRelationalDatabaseFromSnapshot_restoreTime,
    createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName,
    createRelationalDatabaseFromSnapshot_publiclyAccessible,
    createRelationalDatabaseFromSnapshot_availabilityZone,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRelationalDatabaseFromSnapshot' smart constructor.
data CreateRelationalDatabaseFromSnapshot = CreateRelationalDatabaseFromSnapshot'
  { -- | The bundle ID for your new database. A bundle describes the performance
    -- specifications for your database.
    --
    -- You can get a list of database bundle IDs by using the
    -- @get relational database bundles@ operation.
    --
    -- When creating a new database from a snapshot, you cannot choose a bundle
    -- that is smaller than the bundle of the source database.
    relationalDatabaseBundleId :: Core.Maybe Core.Text,
    -- | The name of the source database.
    sourceRelationalDatabaseName :: Core.Maybe Core.Text,
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
    restoreTime :: Core.Maybe Core.POSIX,
    -- | The name of the database snapshot from which to create your new
    -- database.
    relationalDatabaseSnapshotName :: Core.Maybe Core.Text,
    -- | Specifies the accessibility options for your new database. A value of
    -- @true@ specifies a database that is available to resources outside of
    -- your Lightsail account. A value of @false@ specifies a database that is
    -- available only to your Lightsail resources in the same region as your
    -- database.
    publiclyAccessible :: Core.Maybe Core.Bool,
    -- | The Availability Zone in which to create your new database. Use the
    -- @us-east-2a@ case-sensitive format.
    --
    -- You can get a list of Availability Zones by using the @get regions@
    -- operation. Be sure to add the
    -- @include relational database Availability Zones@ parameter to your
    -- request.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | Specifies whether your database is restored from the latest backup time.
    -- A value of @true@ restores from the latest backup time.
    --
    -- Default: @false@
    --
    -- Constraints: Cannot be specified if the @restore time@ parameter is
    -- provided.
    useLatestRestorableTime :: Core.Maybe Core.Bool,
    -- | The name to use for your new database.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'sourceRelationalDatabaseName', 'createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName' - The name of the source database.
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
-- 'relationalDatabaseSnapshotName', 'createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName' - The name of the database snapshot from which to create your new
-- database.
--
-- 'publiclyAccessible', 'createRelationalDatabaseFromSnapshot_publiclyAccessible' - Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
--
-- 'availabilityZone', 'createRelationalDatabaseFromSnapshot_availabilityZone' - The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
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
-- 'relationalDatabaseName', 'createRelationalDatabaseFromSnapshot_relationalDatabaseName' - The name to use for your new database.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
newCreateRelationalDatabaseFromSnapshot ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  CreateRelationalDatabaseFromSnapshot
newCreateRelationalDatabaseFromSnapshot
  pRelationalDatabaseName_ =
    CreateRelationalDatabaseFromSnapshot'
      { relationalDatabaseBundleId =
          Core.Nothing,
        sourceRelationalDatabaseName =
          Core.Nothing,
        restoreTime = Core.Nothing,
        relationalDatabaseSnapshotName =
          Core.Nothing,
        publiclyAccessible = Core.Nothing,
        availabilityZone = Core.Nothing,
        tags = Core.Nothing,
        useLatestRestorableTime =
          Core.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The bundle ID for your new database. A bundle describes the performance
-- specifications for your database.
--
-- You can get a list of database bundle IDs by using the
-- @get relational database bundles@ operation.
--
-- When creating a new database from a snapshot, you cannot choose a bundle
-- that is smaller than the bundle of the source database.
createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Text)
createRelationalDatabaseFromSnapshot_relationalDatabaseBundleId = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseBundleId} -> relationalDatabaseBundleId) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseBundleId = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The name of the source database.
createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Text)
createRelationalDatabaseFromSnapshot_sourceRelationalDatabaseName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {sourceRelationalDatabaseName} -> sourceRelationalDatabaseName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {sourceRelationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)

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
createRelationalDatabaseFromSnapshot_restoreTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.UTCTime)
createRelationalDatabaseFromSnapshot_restoreTime = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {restoreTime} -> restoreTime) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {restoreTime = a} :: CreateRelationalDatabaseFromSnapshot) Core.. Lens.mapping Core._Time

-- | The name of the database snapshot from which to create your new
-- database.
createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Text)
createRelationalDatabaseFromSnapshot_relationalDatabaseSnapshotName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseFromSnapshot)

-- | Specifies the accessibility options for your new database. A value of
-- @true@ specifies a database that is available to resources outside of
-- your Lightsail account. A value of @false@ specifies a database that is
-- available only to your Lightsail resources in the same region as your
-- database.
createRelationalDatabaseFromSnapshot_publiclyAccessible :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Bool)
createRelationalDatabaseFromSnapshot_publiclyAccessible = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {publiclyAccessible} -> publiclyAccessible) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {publiclyAccessible = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The Availability Zone in which to create your new database. Use the
-- @us-east-2a@ case-sensitive format.
--
-- You can get a list of Availability Zones by using the @get regions@
-- operation. Be sure to add the
-- @include relational database Availability Zones@ parameter to your
-- request.
createRelationalDatabaseFromSnapshot_availabilityZone :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Text)
createRelationalDatabaseFromSnapshot_availabilityZone = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {availabilityZone} -> availabilityZone) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {availabilityZone = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createRelationalDatabaseFromSnapshot_tags :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe [Tag])
createRelationalDatabaseFromSnapshot_tags = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {tags} -> tags) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {tags = a} :: CreateRelationalDatabaseFromSnapshot) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether your database is restored from the latest backup time.
-- A value of @true@ restores from the latest backup time.
--
-- Default: @false@
--
-- Constraints: Cannot be specified if the @restore time@ parameter is
-- provided.
createRelationalDatabaseFromSnapshot_useLatestRestorableTime :: Lens.Lens' CreateRelationalDatabaseFromSnapshot (Core.Maybe Core.Bool)
createRelationalDatabaseFromSnapshot_useLatestRestorableTime = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {useLatestRestorableTime} -> useLatestRestorableTime) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {useLatestRestorableTime = a} :: CreateRelationalDatabaseFromSnapshot)

-- | The name to use for your new database.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
createRelationalDatabaseFromSnapshot_relationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseFromSnapshot Core.Text
createRelationalDatabaseFromSnapshot_relationalDatabaseName = Lens.lens (\CreateRelationalDatabaseFromSnapshot' {relationalDatabaseName} -> relationalDatabaseName) (\s@CreateRelationalDatabaseFromSnapshot' {} a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseFromSnapshot)

instance
  Core.AWSRequest
    CreateRelationalDatabaseFromSnapshot
  where
  type
    AWSResponse CreateRelationalDatabaseFromSnapshot =
      CreateRelationalDatabaseFromSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseFromSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateRelationalDatabaseFromSnapshot

instance
  Core.NFData
    CreateRelationalDatabaseFromSnapshot

instance
  Core.ToHeaders
    CreateRelationalDatabaseFromSnapshot
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateRelationalDatabaseFromSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    CreateRelationalDatabaseFromSnapshot
  where
  toJSON CreateRelationalDatabaseFromSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("relationalDatabaseBundleId" Core..=)
              Core.<$> relationalDatabaseBundleId,
            ("sourceRelationalDatabaseName" Core..=)
              Core.<$> sourceRelationalDatabaseName,
            ("restoreTime" Core..=) Core.<$> restoreTime,
            ("relationalDatabaseSnapshotName" Core..=)
              Core.<$> relationalDatabaseSnapshotName,
            ("publiclyAccessible" Core..=)
              Core.<$> publiclyAccessible,
            ("availabilityZone" Core..=)
              Core.<$> availabilityZone,
            ("tags" Core..=) Core.<$> tags,
            ("useLatestRestorableTime" Core..=)
              Core.<$> useLatestRestorableTime,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance
  Core.ToPath
    CreateRelationalDatabaseFromSnapshot
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateRelationalDatabaseFromSnapshot
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRelationalDatabaseFromSnapshotResponse' smart constructor.
data CreateRelationalDatabaseFromSnapshotResponse = CreateRelationalDatabaseFromSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateRelationalDatabaseFromSnapshotResponse
newCreateRelationalDatabaseFromSnapshotResponse
  pHttpStatus_ =
    CreateRelationalDatabaseFromSnapshotResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createRelationalDatabaseFromSnapshotResponse_operations :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse (Core.Maybe [Operation])
createRelationalDatabaseFromSnapshotResponse_operations = Lens.lens (\CreateRelationalDatabaseFromSnapshotResponse' {operations} -> operations) (\s@CreateRelationalDatabaseFromSnapshotResponse' {} a -> s {operations = a} :: CreateRelationalDatabaseFromSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createRelationalDatabaseFromSnapshotResponse_httpStatus :: Lens.Lens' CreateRelationalDatabaseFromSnapshotResponse Core.Int
createRelationalDatabaseFromSnapshotResponse_httpStatus = Lens.lens (\CreateRelationalDatabaseFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateRelationalDatabaseFromSnapshotResponse' {} a -> s {httpStatus = a} :: CreateRelationalDatabaseFromSnapshotResponse)

instance
  Core.NFData
    CreateRelationalDatabaseFromSnapshotResponse
