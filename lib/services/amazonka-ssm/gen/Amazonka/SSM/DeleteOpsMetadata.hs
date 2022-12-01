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
-- Module      : Amazonka.SSM.DeleteOpsMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete OpsMetadata related to an application.
module Amazonka.SSM.DeleteOpsMetadata
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteOpsMetadata' smart constructor.
data DeleteOpsMetadata = DeleteOpsMetadata'
  { -- | The Amazon Resource Name (ARN) of an OpsMetadata Object to delete.
    opsMetadataArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteOpsMetadata
newDeleteOpsMetadata pOpsMetadataArn_ =
  DeleteOpsMetadata'
    { opsMetadataArn =
        pOpsMetadataArn_
    }

-- | The Amazon Resource Name (ARN) of an OpsMetadata Object to delete.
deleteOpsMetadata_opsMetadataArn :: Lens.Lens' DeleteOpsMetadata Prelude.Text
deleteOpsMetadata_opsMetadataArn = Lens.lens (\DeleteOpsMetadata' {opsMetadataArn} -> opsMetadataArn) (\s@DeleteOpsMetadata' {} a -> s {opsMetadataArn = a} :: DeleteOpsMetadata)

instance Core.AWSRequest DeleteOpsMetadata where
  type
    AWSResponse DeleteOpsMetadata =
      DeleteOpsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOpsMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOpsMetadata where
  hashWithSalt _salt DeleteOpsMetadata' {..} =
    _salt `Prelude.hashWithSalt` opsMetadataArn

instance Prelude.NFData DeleteOpsMetadata where
  rnf DeleteOpsMetadata' {..} =
    Prelude.rnf opsMetadataArn

instance Core.ToHeaders DeleteOpsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.DeleteOpsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteOpsMetadata where
  toJSON DeleteOpsMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OpsMetadataArn" Core..= opsMetadataArn)
          ]
      )

instance Core.ToPath DeleteOpsMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteOpsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOpsMetadataResponse' smart constructor.
data DeleteOpsMetadataResponse = DeleteOpsMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteOpsMetadataResponse
newDeleteOpsMetadataResponse pHttpStatus_ =
  DeleteOpsMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOpsMetadataResponse_httpStatus :: Lens.Lens' DeleteOpsMetadataResponse Prelude.Int
deleteOpsMetadataResponse_httpStatus = Lens.lens (\DeleteOpsMetadataResponse' {httpStatus} -> httpStatus) (\s@DeleteOpsMetadataResponse' {} a -> s {httpStatus = a} :: DeleteOpsMetadataResponse)

instance Prelude.NFData DeleteOpsMetadataResponse where
  rnf DeleteOpsMetadataResponse' {..} =
    Prelude.rnf httpStatus
