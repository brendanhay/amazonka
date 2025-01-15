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
-- Module      : Amazonka.Lightsail.CreateRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of your database in Amazon Lightsail. You can use
-- snapshots for backups, to make copies of a database, and to save data
-- before deleting a database.
--
-- The @create relational database snapshot@ operation supports tag-based
-- access control via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateRelationalDatabaseSnapshot
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRelationalDatabaseSnapshot' smart constructor.
data CreateRelationalDatabaseSnapshot = CreateRelationalDatabaseSnapshot'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the database on which to base your new snapshot.
    relationalDatabaseName :: Prelude.Text,
    -- | The name for your new database snapshot.
    --
    -- Constraints:
    --
    -- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
    --
    -- -   The first and last character must be a letter or number.
    relationalDatabaseSnapshotName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'relationalDatabaseSnapshotName'
  Prelude.Text ->
  CreateRelationalDatabaseSnapshot
newCreateRelationalDatabaseSnapshot
  pRelationalDatabaseName_
  pRelationalDatabaseSnapshotName_ =
    CreateRelationalDatabaseSnapshot'
      { tags =
          Prelude.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_,
        relationalDatabaseSnapshotName =
          pRelationalDatabaseSnapshotName_
      }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createRelationalDatabaseSnapshot_tags :: Lens.Lens' CreateRelationalDatabaseSnapshot (Prelude.Maybe [Tag])
createRelationalDatabaseSnapshot_tags = Lens.lens (\CreateRelationalDatabaseSnapshot' {tags} -> tags) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {tags = a} :: CreateRelationalDatabaseSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name of the database on which to base your new snapshot.
createRelationalDatabaseSnapshot_relationalDatabaseName :: Lens.Lens' CreateRelationalDatabaseSnapshot Prelude.Text
createRelationalDatabaseSnapshot_relationalDatabaseName = Lens.lens (\CreateRelationalDatabaseSnapshot' {relationalDatabaseName} -> relationalDatabaseName) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseName = a} :: CreateRelationalDatabaseSnapshot)

-- | The name for your new database snapshot.
--
-- Constraints:
--
-- -   Must contain from 2 to 255 alphanumeric characters, or hyphens.
--
-- -   The first and last character must be a letter or number.
createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName :: Lens.Lens' CreateRelationalDatabaseSnapshot Prelude.Text
createRelationalDatabaseSnapshot_relationalDatabaseSnapshotName = Lens.lens (\CreateRelationalDatabaseSnapshot' {relationalDatabaseSnapshotName} -> relationalDatabaseSnapshotName) (\s@CreateRelationalDatabaseSnapshot' {} a -> s {relationalDatabaseSnapshotName = a} :: CreateRelationalDatabaseSnapshot)

instance
  Core.AWSRequest
    CreateRelationalDatabaseSnapshot
  where
  type
    AWSResponse CreateRelationalDatabaseSnapshot =
      CreateRelationalDatabaseSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRelationalDatabaseSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateRelationalDatabaseSnapshot
  where
  hashWithSalt
    _salt
    CreateRelationalDatabaseSnapshot' {..} =
      _salt
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` relationalDatabaseName
        `Prelude.hashWithSalt` relationalDatabaseSnapshotName

instance
  Prelude.NFData
    CreateRelationalDatabaseSnapshot
  where
  rnf CreateRelationalDatabaseSnapshot' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf relationalDatabaseName `Prelude.seq`
        Prelude.rnf relationalDatabaseSnapshotName

instance
  Data.ToHeaders
    CreateRelationalDatabaseSnapshot
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateRelationalDatabaseSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRelationalDatabaseSnapshot where
  toJSON CreateRelationalDatabaseSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              ),
            Prelude.Just
              ( "relationalDatabaseSnapshotName"
                  Data..= relationalDatabaseSnapshotName
              )
          ]
      )

instance Data.ToPath CreateRelationalDatabaseSnapshot where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateRelationalDatabaseSnapshot
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRelationalDatabaseSnapshotResponse' smart constructor.
data CreateRelationalDatabaseSnapshotResponse = CreateRelationalDatabaseSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateRelationalDatabaseSnapshotResponse
newCreateRelationalDatabaseSnapshotResponse
  pHttpStatus_ =
    CreateRelationalDatabaseSnapshotResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createRelationalDatabaseSnapshotResponse_operations :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse (Prelude.Maybe [Operation])
createRelationalDatabaseSnapshotResponse_operations = Lens.lens (\CreateRelationalDatabaseSnapshotResponse' {operations} -> operations) (\s@CreateRelationalDatabaseSnapshotResponse' {} a -> s {operations = a} :: CreateRelationalDatabaseSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createRelationalDatabaseSnapshotResponse_httpStatus :: Lens.Lens' CreateRelationalDatabaseSnapshotResponse Prelude.Int
createRelationalDatabaseSnapshotResponse_httpStatus = Lens.lens (\CreateRelationalDatabaseSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateRelationalDatabaseSnapshotResponse' {} a -> s {httpStatus = a} :: CreateRelationalDatabaseSnapshotResponse)

instance
  Prelude.NFData
    CreateRelationalDatabaseSnapshotResponse
  where
  rnf CreateRelationalDatabaseSnapshotResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
