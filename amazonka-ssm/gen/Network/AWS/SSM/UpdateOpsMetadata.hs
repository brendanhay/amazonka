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
-- Module      : Network.AWS.SSM.UpdateOpsMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Systems Manager calls this API action when you edit OpsMetadata in
-- Application Manager.
module Network.AWS.SSM.UpdateOpsMetadata
  ( -- * Creating a Request
    UpdateOpsMetadata (..),
    newUpdateOpsMetadata,

    -- * Request Lenses
    updateOpsMetadata_metadataToUpdate,
    updateOpsMetadata_keysToDelete,
    updateOpsMetadata_opsMetadataArn,

    -- * Destructuring the Response
    UpdateOpsMetadataResponse (..),
    newUpdateOpsMetadataResponse,

    -- * Response Lenses
    updateOpsMetadataResponse_opsMetadataArn,
    updateOpsMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateOpsMetadata' smart constructor.
data UpdateOpsMetadata = UpdateOpsMetadata'
  { -- | Metadata to add to an OpsMetadata object.
    metadataToUpdate :: Core.Maybe (Core.HashMap Core.Text MetadataValue),
    -- | The metadata keys to delete from the OpsMetadata object.
    keysToDelete :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The Amazon Resoure Name (ARN) of the OpsMetadata Object to update.
    opsMetadataArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateOpsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataToUpdate', 'updateOpsMetadata_metadataToUpdate' - Metadata to add to an OpsMetadata object.
--
-- 'keysToDelete', 'updateOpsMetadata_keysToDelete' - The metadata keys to delete from the OpsMetadata object.
--
-- 'opsMetadataArn', 'updateOpsMetadata_opsMetadataArn' - The Amazon Resoure Name (ARN) of the OpsMetadata Object to update.
newUpdateOpsMetadata ::
  -- | 'opsMetadataArn'
  Core.Text ->
  UpdateOpsMetadata
newUpdateOpsMetadata pOpsMetadataArn_ =
  UpdateOpsMetadata'
    { metadataToUpdate = Core.Nothing,
      keysToDelete = Core.Nothing,
      opsMetadataArn = pOpsMetadataArn_
    }

-- | Metadata to add to an OpsMetadata object.
updateOpsMetadata_metadataToUpdate :: Lens.Lens' UpdateOpsMetadata (Core.Maybe (Core.HashMap Core.Text MetadataValue))
updateOpsMetadata_metadataToUpdate = Lens.lens (\UpdateOpsMetadata' {metadataToUpdate} -> metadataToUpdate) (\s@UpdateOpsMetadata' {} a -> s {metadataToUpdate = a} :: UpdateOpsMetadata) Core.. Lens.mapping Lens._Coerce

-- | The metadata keys to delete from the OpsMetadata object.
updateOpsMetadata_keysToDelete :: Lens.Lens' UpdateOpsMetadata (Core.Maybe (Core.NonEmpty Core.Text))
updateOpsMetadata_keysToDelete = Lens.lens (\UpdateOpsMetadata' {keysToDelete} -> keysToDelete) (\s@UpdateOpsMetadata' {} a -> s {keysToDelete = a} :: UpdateOpsMetadata) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resoure Name (ARN) of the OpsMetadata Object to update.
updateOpsMetadata_opsMetadataArn :: Lens.Lens' UpdateOpsMetadata Core.Text
updateOpsMetadata_opsMetadataArn = Lens.lens (\UpdateOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@UpdateOpsMetadata' {} a -> s {opsMetadataArn = a} :: UpdateOpsMetadata)

instance Core.AWSRequest UpdateOpsMetadata where
  type
    AWSResponse UpdateOpsMetadata =
      UpdateOpsMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOpsMetadataResponse'
            Core.<$> (x Core..?> "OpsMetadataArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateOpsMetadata

instance Core.NFData UpdateOpsMetadata

instance Core.ToHeaders UpdateOpsMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.UpdateOpsMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateOpsMetadata where
  toJSON UpdateOpsMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MetadataToUpdate" Core..=)
              Core.<$> metadataToUpdate,
            ("KeysToDelete" Core..=) Core.<$> keysToDelete,
            Core.Just ("OpsMetadataArn" Core..= opsMetadataArn)
          ]
      )

instance Core.ToPath UpdateOpsMetadata where
  toPath = Core.const "/"

instance Core.ToQuery UpdateOpsMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateOpsMetadataResponse' smart constructor.
data UpdateOpsMetadataResponse = UpdateOpsMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object that was
    -- updated.
    opsMetadataArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateOpsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsMetadataArn', 'updateOpsMetadataResponse_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object that was
-- updated.
--
-- 'httpStatus', 'updateOpsMetadataResponse_httpStatus' - The response's http status code.
newUpdateOpsMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateOpsMetadataResponse
newUpdateOpsMetadataResponse pHttpStatus_ =
  UpdateOpsMetadataResponse'
    { opsMetadataArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object that was
-- updated.
updateOpsMetadataResponse_opsMetadataArn :: Lens.Lens' UpdateOpsMetadataResponse (Core.Maybe Core.Text)
updateOpsMetadataResponse_opsMetadataArn = Lens.lens (\UpdateOpsMetadataResponse' {opsMetadataArn} -> opsMetadataArn) (\s@UpdateOpsMetadataResponse' {} a -> s {opsMetadataArn = a} :: UpdateOpsMetadataResponse)

-- | The response's http status code.
updateOpsMetadataResponse_httpStatus :: Lens.Lens' UpdateOpsMetadataResponse Core.Int
updateOpsMetadataResponse_httpStatus = Lens.lens (\UpdateOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateOpsMetadataResponse' {} a -> s {httpStatus = a} :: UpdateOpsMetadataResponse)

instance Core.NFData UpdateOpsMetadataResponse
