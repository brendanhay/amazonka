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
-- Module      : Network.AWS.SSM.DeleteOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete OpsMetadata related to an application.
module Network.AWS.SSM.DeleteOpsMetadata
  ( -- * Creating a Request
    DeleteOpsMetadata (..),
    newDeleteOpsMetadata,

    -- * Request Lenses
    deleteOpsMetadata_opsMetadataArn,

    -- * Destructuring the Response
    DeleteOpsMetadataResponse (..),
    newDeleteOpsMetadataResponse,

    -- * Response Lenses
    deleteOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteOpsMetadata' smart constructor.
data DeleteOpsMetadata = DeleteOpsMetadata'
  { -- | The Amazon Resource Name (ARN) of an OpsMetadata Object to delete.
    opsMetadataArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsMetadataArn', 'deleteOpsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of an OpsMetadata Object to delete.
newDeleteOpsMetadata ::
  -- | 'opsMetadataArn'
  Core.Text ->
  DeleteOpsMetadata
newDeleteOpsMetadata pOpsMetadataArn_ =
  DeleteOpsMetadata'
    { opsMetadataArn =
        pOpsMetadataArn_
    }

-- | The Amazon Resource Name (ARN) of an OpsMetadata Object to delete.
deleteOpsMetadata_opsMetadataArn :: Lens.Lens' DeleteOpsMetadata Core.Text
deleteOpsMetadata_opsMetadataArn = Lens.lens (\DeleteOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@DeleteOpsMetadata' {} a -> s {opsMetadataArn = a} :: DeleteOpsMetadata)

instance Core.AWSRequest DeleteOpsMetadata where
  type
    AWSResponse DeleteOpsMetadata =
      DeleteOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOpsMetadataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteOpsMetadata

instance Core.NFData DeleteOpsMetadata

instance Core.ToHeaders DeleteOpsMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DeleteOpsMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteOpsMetadata where
  toJSON DeleteOpsMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("OpsMetadataArn" Core..= opsMetadataArn)
          ]
      )

instance Core.ToPath DeleteOpsMetadata where
  toPath = Core.const "/"

instance Core.ToQuery DeleteOpsMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteOpsMetadataResponse' smart constructor.
data DeleteOpsMetadataResponse = DeleteOpsMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOpsMetadataResponse_httpStatus' - The response's http status code.
newDeleteOpsMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteOpsMetadataResponse
newDeleteOpsMetadataResponse pHttpStatus_ =
  DeleteOpsMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOpsMetadataResponse_httpStatus :: Lens.Lens' DeleteOpsMetadataResponse Core.Int
deleteOpsMetadataResponse_httpStatus = Lens.lens (\DeleteOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@DeleteOpsMetadataResponse' {} a -> s {httpStatus = a} :: DeleteOpsMetadataResponse)

instance Core.NFData DeleteOpsMetadataResponse
