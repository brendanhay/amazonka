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
-- Module      : Amazonka.SSM.UpdateOpsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Web Services Systems Manager calls this API operation when you
-- edit OpsMetadata in Application Manager.
module Amazonka.SSM.UpdateOpsMetadata
  ( -- * Creating a Request
    UpdateOpsMetadata (..),
    newUpdateOpsMetadata,

    -- * Request Lenses
    updateOpsMetadata_keysToDelete,
    updateOpsMetadata_metadataToUpdate,
    updateOpsMetadata_opsMetadataArn,

    -- * Destructuring the Response
    UpdateOpsMetadataResponse (..),
    newUpdateOpsMetadataResponse,

    -- * Response Lenses
    updateOpsMetadataResponse_opsMetadataArn,
    updateOpsMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateOpsMetadata' smart constructor.
data UpdateOpsMetadata = UpdateOpsMetadata'
  { -- | The metadata keys to delete from the OpsMetadata object.
    keysToDelete :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Metadata to add to an OpsMetadata object.
    metadataToUpdate :: Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue),
    -- | The Amazon Resource Name (ARN) of the OpsMetadata Object to update.
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
-- 'keysToDelete', 'updateOpsMetadata_keysToDelete' - The metadata keys to delete from the OpsMetadata object.
--
-- 'metadataToUpdate', 'updateOpsMetadata_metadataToUpdate' - Metadata to add to an OpsMetadata object.
--
-- 'opsMetadataArn', 'updateOpsMetadata_opsMetadataArn' - The Amazon Resource Name (ARN) of the OpsMetadata Object to update.
newUpdateOpsMetadata ::
  -- | 'opsMetadataArn'
  Prelude.Text ->
  UpdateOpsMetadata
newUpdateOpsMetadata pOpsMetadataArn_ =
  UpdateOpsMetadata'
    { keysToDelete = Prelude.Nothing,
      metadataToUpdate = Prelude.Nothing,
      opsMetadataArn = pOpsMetadataArn_
    }

-- | The metadata keys to delete from the OpsMetadata object.
updateOpsMetadata_keysToDelete :: Lens.Lens' UpdateOpsMetadata (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateOpsMetadata_keysToDelete = Lens.lens (\UpdateOpsMetadata' {keysToDelete} -> keysToDelete) (\s@UpdateOpsMetadata' {} a -> s {keysToDelete = a} :: UpdateOpsMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Metadata to add to an OpsMetadata object.
updateOpsMetadata_metadataToUpdate :: Lens.Lens' UpdateOpsMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text MetadataValue))
updateOpsMetadata_metadataToUpdate = Lens.lens (\UpdateOpsMetadata' {metadataToUpdate} -> metadataToUpdate) (\s@UpdateOpsMetadata' {} a -> s {metadataToUpdate = a} :: UpdateOpsMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the OpsMetadata Object to update.
updateOpsMetadata_opsMetadataArn :: Lens.Lens' UpdateOpsMetadata Prelude.Text
updateOpsMetadata_opsMetadataArn = Lens.lens (\UpdateOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@UpdateOpsMetadata' {} a -> s {opsMetadataArn = a} :: UpdateOpsMetadata)

instance Core.AWSRequest UpdateOpsMetadata where
  type
    AWSResponse UpdateOpsMetadata =
      UpdateOpsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateOpsMetadataResponse'
            Prelude.<$> (x Data..?> "OpsMetadataArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateOpsMetadata where
  hashWithSalt _salt UpdateOpsMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` keysToDelete
      `Prelude.hashWithSalt` metadataToUpdate
      `Prelude.hashWithSalt` opsMetadataArn

instance Prelude.NFData UpdateOpsMetadata where
  rnf UpdateOpsMetadata' {..} =
    Prelude.rnf keysToDelete
      `Prelude.seq` Prelude.rnf metadataToUpdate
      `Prelude.seq` Prelude.rnf opsMetadataArn

instance Data.ToHeaders UpdateOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateOpsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateOpsMetadata where
  toJSON UpdateOpsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeysToDelete" Data..=) Prelude.<$> keysToDelete,
            ("MetadataToUpdate" Data..=)
              Prelude.<$> metadataToUpdate,
            Prelude.Just
              ("OpsMetadataArn" Data..= opsMetadataArn)
          ]
      )

instance Data.ToPath UpdateOpsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateOpsMetadata where
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

instance Prelude.NFData UpdateOpsMetadataResponse where
  rnf UpdateOpsMetadataResponse' {..} =
    Prelude.rnf opsMetadataArn
      `Prelude.seq` Prelude.rnf httpStatus
