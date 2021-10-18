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
-- Amazon Web Services Systems Manager calls this API operation when you
-- edit OpsMetadata in Application Manager.
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateOpsMetadata' smart constructor.
data UpdateOpsMetadata = UpdateOpsMetadata'
  { -- | Metadata to add to an OpsMetadata object.
    metadataToUpdate :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue),
    -- | The metadata keys to delete from the OpsMetadata object.
    keysToDelete :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resoure Name (ARN) of the OpsMetadata Object to update.
    opsMetadataArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateOpsMetadata
newUpdateOpsMetadata pOpsMetadataArn_ =
  UpdateOpsMetadata'
    { metadataToUpdate =
        Prelude.Nothing,
      keysToDelete = Prelude.Nothing,
      opsMetadataArn = pOpsMetadataArn_
    }

-- | Metadata to add to an OpsMetadata object.
updateOpsMetadata_metadataToUpdate :: Lens.Lens' UpdateOpsMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue))
updateOpsMetadata_metadataToUpdate = Lens.lens (\UpdateOpsMetadata' {metadataToUpdate} -> metadataToUpdate) (\s@UpdateOpsMetadata' {} a -> s {metadataToUpdate = a} :: UpdateOpsMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | The metadata keys to delete from the OpsMetadata object.
updateOpsMetadata_keysToDelete :: Lens.Lens' UpdateOpsMetadata (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateOpsMetadata_keysToDelete = Lens.lens (\UpdateOpsMetadata' {keysToDelete} -> keysToDelete) (\s@UpdateOpsMetadata' {} a -> s {keysToDelete = a} :: UpdateOpsMetadata) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resoure Name (ARN) of the OpsMetadata Object to update.
updateOpsMetadata_opsMetadataArn :: Lens.Lens' UpdateOpsMetadata Prelude.Text
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
            Prelude.<$> (x Core..?> "OpsMetadataArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOpsMetadata

instance Prelude.NFData UpdateOpsMetadata

instance Core.ToHeaders UpdateOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateOpsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateOpsMetadata where
  toJSON UpdateOpsMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetadataToUpdate" Core..=)
              Prelude.<$> metadataToUpdate,
            ("KeysToDelete" Core..=) Prelude.<$> keysToDelete,
            Prelude.Just
              ("OpsMetadataArn" Core..= opsMetadataArn)
          ]
      )

instance Core.ToPath UpdateOpsMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOpsMetadataResponse' smart constructor.
data UpdateOpsMetadataResponse = UpdateOpsMetadataResponse'
  { -- | The Amazon Resource Name (ARN) of the OpsMetadata Object that was
    -- updated.
    opsMetadataArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateOpsMetadataResponse
newUpdateOpsMetadataResponse pHttpStatus_ =
  UpdateOpsMetadataResponse'
    { opsMetadataArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object that was
-- updated.
updateOpsMetadataResponse_opsMetadataArn :: Lens.Lens' UpdateOpsMetadataResponse (Prelude.Maybe Prelude.Text)
updateOpsMetadataResponse_opsMetadataArn = Lens.lens (\UpdateOpsMetadataResponse' {opsMetadataArn} -> opsMetadataArn) (\s@UpdateOpsMetadataResponse' {} a -> s {opsMetadataArn = a} :: UpdateOpsMetadataResponse)

-- | The response's http status code.
updateOpsMetadataResponse_httpStatus :: Lens.Lens' UpdateOpsMetadataResponse Prelude.Int
updateOpsMetadataResponse_httpStatus = Lens.lens (\UpdateOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateOpsMetadataResponse' {} a -> s {httpStatus = a} :: UpdateOpsMetadataResponse)

instance Prelude.NFData UpdateOpsMetadataResponse
