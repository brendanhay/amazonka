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
-- Module      : Network.AWS.SSM.CreateOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If you create a new application in Application Manager, Systems Manager
-- calls this API action to specify information about the new application,
-- including the application type.
module Network.AWS.SSM.CreateOpsMetadata
  ( -- * Creating a Request
    CreateOpsMetadata (..),
    newCreateOpsMetadata,

    -- * Request Lenses
    createOpsMetadata_metadata,
    createOpsMetadata_resourceId,

    -- * Destructuring the Response
    CreateOpsMetadataResponse (..),
    newCreateOpsMetadataResponse,

    -- * Response Lenses
    createOpsMetadataResponse_opsMetadataArn,
    createOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreateOpsMetadata' smart constructor.
data CreateOpsMetadata = CreateOpsMetadata'
  { -- | Metadata for a new Application Manager application.
    metadata :: Core.Maybe (Core.HashMap Core.Text MetadataValue),
    -- | A resource ID for a new Application Manager application.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadata', 'createOpsMetadata_metadata' - Metadata for a new Application Manager application.
--
-- 'resourceId', 'createOpsMetadata_resourceId' - A resource ID for a new Application Manager application.
newCreateOpsMetadata ::
  -- | 'resourceId'
  Core.Text ->
  CreateOpsMetadata
newCreateOpsMetadata pResourceId_ =
  CreateOpsMetadata'
    { metadata = Core.Nothing,
      resourceId = pResourceId_
    }

-- | Metadata for a new Application Manager application.
createOpsMetadata_metadata :: Lens.Lens' CreateOpsMetadata (Core.Maybe (Core.HashMap Core.Text MetadataValue))
createOpsMetadata_metadata = Lens.lens (\CreateOpsMetadata' {metadata} -> metadata) (\s@CreateOpsMetadata' {} a -> s {metadata = a} :: CreateOpsMetadata) Core.. Lens.mapping Lens._Coerce

-- | A resource ID for a new Application Manager application.
createOpsMetadata_resourceId :: Lens.Lens' CreateOpsMetadata Core.Text
createOpsMetadata_resourceId = Lens.lens (\CreateOpsMetadata' {resourceId} -> resourceId) (\s@CreateOpsMetadata' {} a -> s {resourceId = a} :: CreateOpsMetadata)

instance Core.AWSRequest CreateOpsMetadata where
  type
    AWSResponse CreateOpsMetadata =
      CreateOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOpsMetadataResponse'
            Core.<$> (x Core..?> "OpsMetadataArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOpsMetadata

instance Core.NFData CreateOpsMetadata

instance Core.ToHeaders CreateOpsMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreateOpsMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateOpsMetadata where
  toJSON CreateOpsMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Metadata" Core..=) Core.<$> metadata,
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath CreateOpsMetadata where
  toPath = Core.const "/"

instance Core.ToQuery CreateOpsMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOpsMetadataResponse' smart constructor.
data CreateOpsMetadataResponse = CreateOpsMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
    -- by the call.
    opsMetadataArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsMetadataArn', 'createOpsMetadataResponse_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
-- by the call.
--
-- 'httpStatus', 'createOpsMetadataResponse_httpStatus' - The response's http status code.
newCreateOpsMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOpsMetadataResponse
newCreateOpsMetadataResponse pHttpStatus_ =
  CreateOpsMetadataResponse'
    { opsMetadataArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object or blob created
-- by the call.
createOpsMetadataResponse_opsMetadataArn :: Lens.Lens' CreateOpsMetadataResponse (Core.Maybe Core.Text)
createOpsMetadataResponse_opsMetadataArn = Lens.lens (\CreateOpsMetadataResponse' {opsMetadataArn} -> opsMetadataArn) (\s@CreateOpsMetadataResponse' {} a -> s {opsMetadataArn = a} :: CreateOpsMetadataResponse)

-- | The response's http status code.
createOpsMetadataResponse_httpStatus :: Lens.Lens' CreateOpsMetadataResponse Core.Int
createOpsMetadataResponse_httpStatus = Lens.lens (\CreateOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@CreateOpsMetadataResponse' {} a -> s {httpStatus = a} :: CreateOpsMetadataResponse)

instance Core.NFData CreateOpsMetadataResponse
