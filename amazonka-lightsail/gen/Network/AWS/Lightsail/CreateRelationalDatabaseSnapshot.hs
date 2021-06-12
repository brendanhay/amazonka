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
-- Module      : Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of your database in Amazon Lightsail. You can use
-- snapshots for backups, to make copies of a database, and to save data
-- before deleting a database.
--
-- The @create relational database snapshot@ operation supports tag-based
-- access control via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
  ( -- * Creating a Request
    CreateRelationalDatabaseSnapshot (..),
    newCreateRelationalDatabaseSnapshot,

    -- * Request Lenses
    createRelationalDatabaseSnapshot_tags,
    createRelationalDatabaseSnapshot_relationalDatabaseName,
    createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName,

    -- * Destructuring the Response
    CreateRelationalDatabaseSnapshotResponse (..),
    newCreateRelationalDatabaseSnapshotResponse,

    -- * Response Lenses
    createRelationalDatabaseSnapshotResponse_operations,
    createRelationalDatabaseSnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRelationalDatabaseSnapshot' smart constructor.
data CreateRelationalDatabaseSnapshot = CreateRelationalDatabaseSnapshot'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The name of the database on which to base your new snapshot.
    relationalDatabaseName :: Core.Text,
    -- | The name for your new database snapshot.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    relationalDatabaseSnapshotName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRelationalDatabaseSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'relationalDatabaseName', 'createRelationalDatabaseSnapshot_relationalDatabaseName' - The name of the database on which to base your new snapshot.
--
-- 'relationalDatabaseSnapshotName', 'createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName' - The name for your new database snapshot.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
newCreateRelationalDatabaseSnapshot ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  -- | 'relationalDatabaseSnapshotName'
  Core.Text ->
  CreateRelationalDatabaseSnapshot
newCreateRelationalDatabaseSnapshot
  pRelationalDatabaseName_
  pRelationalDatabaseSnapshotName_ =
    CreateRelationalDatabaseSnapshot'
      { tags =
          Core.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_,
        relationalDatabaseSnapshotName =
          pRelationalDatabaseSnapshotName_
      }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createRelationalDatabaseSnapshot_tags :: Lens.Lens' CreateRelationalDatabaseSnapshot (Core.Maybe [Tag])
createRelationalDatabaseSnapshot_tags = Lens.lens (\CreateRelationalDatabaseSnapshot' {tags} -> tags) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {tags = a} :: CreateRelationalDatabaseSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The name of the database on which to base your new snapshot.
createRelationalDatabaseSnapshot_relationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseSnapshot Core.Text
createRelationalDatabaseSnapshot_relationalDatabaseName = Lens.lens (\CreateRelationalDatabaseSnapshot' {relationalDatabaseName} -> relationalDatabaseName) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseSnapshot)

-- | The name for your new database snapshot.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseSnapshot Core.Text
createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName = Lens.lens (\CreateRelationalDatabaseSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseSnapshot)

instance
  Core.AWSRequest
    CreateRelationalDatabaseSnapshot
  where
  type
    AWSResponse CreateRelationalDatabaseSnapshot =
      CreateRelationalDatabaseSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateRelationalDatabaseSnapshot

instance Core.NFData CreateRelationalDatabaseSnapshot

instance
  Core.ToHeaders
    CreateRelationalDatabaseSnapshot
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateRelationalDatabaseSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRelationalDatabaseSnapshot where
  toJSON CreateRelationalDatabaseSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              ),
            Core.Just
              ( "relationalDatabaseSnapshotName"
                  Core..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Core.ToPath CreateRelationalDatabaseSnapshot where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateRelationalDatabaseSnapshot
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRelationalDatabaseSnapshotResponse' smart constructor.
data CreateRelationalDatabaseSnapshotResponse = CreateRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRelationalDatabaseSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createRelationalDatabaseSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createRelationalDatabaseSnapshotResponse_httpStatus' - The response's http status code.
newCreateRelationalDatabaseSnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateRelationalDatabaseSnapshotResponse
newCreateRelationalDatabaseSnapshotResponse
  pHttpStatus_ =
    CreateRelationalDatabaseSnapshotResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createRelationalDatabaseSnapshotResponse_operations :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse (Core.Maybe [Operation])
createRelationalDatabaseSnapshotResponse_operations = Lens.lens (\CreateRelationalDatabaseSnapshotResponse' {operations} -> operations) (\s@CreateRelationalDatabaseSnapshotResponse' {} a -> s {operations = a} :: CreateRelationalDatabaseSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createRelationalDatabaseSnapshotResponse_httpStatus :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse Core.Int
createRelationalDatabaseSnapshotResponse_httpStatus = Lens.lens (\CreateRelationalDatabaseSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateRelationalDatabaseSnapshotResponse' {} a -> s {httpStatus = a} :: CreateRelationalDatabaseSnapshotResponse)

instance
  Core.NFData
    CreateRelationalDatabaseSnapshotResponse
