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
-- Module      : Amazonka.Lightsail.CreateInstanceSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a specific virtual private server, or /instance/.
-- You can use a snapshot to create a new instance that is based on that
-- snapshot.
--
-- The @create instance snapshot@ operation supports tag-based access
-- control via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.CreateInstanceSnapshot
  ( -- * Creating a Request
    CreateInstanceSnapshot (..),
    newCreateInstanceSnapshot,

    -- * Request Lenses
    createInstanceSnapshot_tags,
    createInstanceSnapshot_instanceSnapshotName,
    createInstanceSnapshot_instanceName,

    -- * Destructuring the Response
    CreateInstanceSnapshotResponse (..),
    newCreateInstanceSnapshotResponse,

    -- * Response Lenses
    createInstanceSnapshotResponse_operations,
    createInstanceSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceSnapshot' smart constructor.
data CreateInstanceSnapshot = CreateInstanceSnapshot'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name for your new snapshot.
    instanceSnapshotName :: Prelude.Text,
    -- | The Lightsail instance on which to base your snapshot.
    instanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createInstanceSnapshot_tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'instanceSnapshotName', 'createInstanceSnapshot_instanceSnapshotName' - The name for your new snapshot.
--
-- 'instanceName', 'createInstanceSnapshot_instanceName' - The Lightsail instance on which to base your snapshot.
newCreateInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Prelude.Text ->
  -- | 'instanceName'
  Prelude.Text ->
  CreateInstanceSnapshot
newCreateInstanceSnapshot
  pInstanceSnapshotName_
  pInstanceName_ =
    CreateInstanceSnapshot'
      { tags = Prelude.Nothing,
        instanceSnapshotName = pInstanceSnapshotName_,
        instanceName = pInstanceName_
      }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createInstanceSnapshot_tags :: Lens.Lens' CreateInstanceSnapshot (Prelude.Maybe [Tag])
createInstanceSnapshot_tags = Lens.lens (\CreateInstanceSnapshot' {tags} -> tags) (\s@CreateInstanceSnapshot' {} a -> s {tags = a} :: CreateInstanceSnapshot) Prelude.. Lens.mapping Lens.coerced

-- | The name for your new snapshot.
createInstanceSnapshot_instanceSnapshotName :: Lens.Lens' CreateInstanceSnapshot Prelude.Text
createInstanceSnapshot_instanceSnapshotName = Lens.lens (\CreateInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@CreateInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: CreateInstanceSnapshot)

-- | The Lightsail instance on which to base your snapshot.
createInstanceSnapshot_instanceName :: Lens.Lens' CreateInstanceSnapshot Prelude.Text
createInstanceSnapshot_instanceName = Lens.lens (\CreateInstanceSnapshot' {instanceName} -> instanceName) (\s@CreateInstanceSnapshot' {} a -> s {instanceName = a} :: CreateInstanceSnapshot)

instance Core.AWSRequest CreateInstanceSnapshot where
  type
    AWSResponse CreateInstanceSnapshot =
      CreateInstanceSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceSnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateInstanceSnapshot where
  hashWithSalt _salt CreateInstanceSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` instanceSnapshotName
      `Prelude.hashWithSalt` instanceName

instance Prelude.NFData CreateInstanceSnapshot where
  rnf CreateInstanceSnapshot' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf instanceSnapshotName
      `Prelude.seq` Prelude.rnf instanceName

instance Data.ToHeaders CreateInstanceSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CreateInstanceSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInstanceSnapshot where
  toJSON CreateInstanceSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "instanceSnapshotName"
                  Data..= instanceSnapshotName
              ),
            Prelude.Just ("instanceName" Data..= instanceName)
          ]
      )

instance Data.ToPath CreateInstanceSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInstanceSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInstanceSnapshotResponse' smart constructor.
data CreateInstanceSnapshotResponse = CreateInstanceSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createInstanceSnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createInstanceSnapshotResponse_httpStatus' - The response's http status code.
newCreateInstanceSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceSnapshotResponse
newCreateInstanceSnapshotResponse pHttpStatus_ =
  CreateInstanceSnapshotResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createInstanceSnapshotResponse_operations :: Lens.Lens' CreateInstanceSnapshotResponse (Prelude.Maybe [Operation])
createInstanceSnapshotResponse_operations = Lens.lens (\CreateInstanceSnapshotResponse' {operations} -> operations) (\s@CreateInstanceSnapshotResponse' {} a -> s {operations = a} :: CreateInstanceSnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createInstanceSnapshotResponse_httpStatus :: Lens.Lens' CreateInstanceSnapshotResponse Prelude.Int
createInstanceSnapshotResponse_httpStatus = Lens.lens (\CreateInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: CreateInstanceSnapshotResponse)

instance
  Prelude.NFData
    CreateInstanceSnapshotResponse
  where
  rnf CreateInstanceSnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
