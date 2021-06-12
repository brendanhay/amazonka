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
-- Module      : Network.AWS.Lightsail.CreateInstanceSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a specific virtual private server, or /instance/.
-- You can use a snapshot to create a new instance that is based on that
-- snapshot.
--
-- The @create instance snapshot@ operation supports tag-based access
-- control via request tags. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.CreateInstanceSnapshot
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInstanceSnapshot' smart constructor.
data CreateInstanceSnapshot = CreateInstanceSnapshot'
  { -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | The name for your new snapshot.
    instanceSnapshotName :: Core.Text,
    -- | The Lightsail instance on which to base your snapshot.
    instanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'instanceName'
  Core.Text ->
  CreateInstanceSnapshot
newCreateInstanceSnapshot
  pInstanceSnapshotName_
  pInstanceName_ =
    CreateInstanceSnapshot'
      { tags = Core.Nothing,
        instanceSnapshotName = pInstanceSnapshotName_,
        instanceName = pInstanceName_
      }

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createInstanceSnapshot_tags :: Lens.Lens' CreateInstanceSnapshot (Core.Maybe [Tag])
createInstanceSnapshot_tags = Lens.lens (\CreateInstanceSnapshot' {tags} -> tags) (\s@CreateInstanceSnapshot' {} a -> s {tags = a} :: CreateInstanceSnapshot) Core.. Lens.mapping Lens._Coerce

-- | The name for your new snapshot.
createInstanceSnapshot_instanceSnapshotName :: Lens.Lens' CreateInstanceSnapshot Core.Text
createInstanceSnapshot_instanceSnapshotName = Lens.lens (\CreateInstanceSnapshot' {instanceSnapshotName} -> instanceSnapshotName) (\s@CreateInstanceSnapshot' {} a -> s {instanceSnapshotName = a} :: CreateInstanceSnapshot)

-- | The Lightsail instance on which to base your snapshot.
createInstanceSnapshot_instanceName :: Lens.Lens' CreateInstanceSnapshot Core.Text
createInstanceSnapshot_instanceName = Lens.lens (\CreateInstanceSnapshot' {instanceName} -> instanceName) (\s@CreateInstanceSnapshot' {} a -> s {instanceName = a} :: CreateInstanceSnapshot)

instance Core.AWSRequest CreateInstanceSnapshot where
  type
    AWSResponse CreateInstanceSnapshot =
      CreateInstanceSnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInstanceSnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInstanceSnapshot

instance Core.NFData CreateInstanceSnapshot

instance Core.ToHeaders CreateInstanceSnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateInstanceSnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateInstanceSnapshot where
  toJSON CreateInstanceSnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            Core.Just
              ( "instanceSnapshotName"
                  Core..= instanceSnapshotName
              ),
            Core.Just ("instanceName" Core..= instanceName)
          ]
      )

instance Core.ToPath CreateInstanceSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CreateInstanceSnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateInstanceSnapshotResponse' smart constructor.
data CreateInstanceSnapshotResponse = CreateInstanceSnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateInstanceSnapshotResponse
newCreateInstanceSnapshotResponse pHttpStatus_ =
  CreateInstanceSnapshotResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createInstanceSnapshotResponse_operations :: Lens.Lens' CreateInstanceSnapshotResponse (Core.Maybe [Operation])
createInstanceSnapshotResponse_operations = Lens.lens (\CreateInstanceSnapshotResponse' {operations} -> operations) (\s@CreateInstanceSnapshotResponse' {} a -> s {operations = a} :: CreateInstanceSnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createInstanceSnapshotResponse_httpStatus :: Lens.Lens' CreateInstanceSnapshotResponse Core.Int
createInstanceSnapshotResponse_httpStatus = Lens.lens (\CreateInstanceSnapshotResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceSnapshotResponse' {} a -> s {httpStatus = a} :: CreateInstanceSnapshotResponse)

instance Core.NFData CreateInstanceSnapshotResponse
