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
-- Module      : Amazonka.MacieV2.UpdateClassificationScope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the classification scope settings for an account.
module Amazonka.MacieV2.UpdateClassificationScope
  ( -- * Creating a Request
    UpdateClassificationScope (..),
    newUpdateClassificationScope,

    -- * Request Lenses
    updateClassificationScope_s3,
    updateClassificationScope_id,

    -- * Destructuring the Response
    UpdateClassificationScopeResponse (..),
    newUpdateClassificationScopeResponse,

    -- * Response Lenses
    updateClassificationScopeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateClassificationScope' smart constructor.
data UpdateClassificationScope = UpdateClassificationScope'
  { -- | The S3 buckets to add or remove from the exclusion list defined by the
    -- classification scope.
    s3 :: Prelude.Maybe S3ClassificationScopeUpdate,
    -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassificationScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3', 'updateClassificationScope_s3' - The S3 buckets to add or remove from the exclusion list defined by the
-- classification scope.
--
-- 'id', 'updateClassificationScope_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newUpdateClassificationScope ::
  -- | 'id'
  Prelude.Text ->
  UpdateClassificationScope
newUpdateClassificationScope pId_ =
  UpdateClassificationScope'
    { s3 = Prelude.Nothing,
      id = pId_
    }

-- | The S3 buckets to add or remove from the exclusion list defined by the
-- classification scope.
updateClassificationScope_s3 :: Lens.Lens' UpdateClassificationScope (Prelude.Maybe S3ClassificationScopeUpdate)
updateClassificationScope_s3 = Lens.lens (\UpdateClassificationScope' {s3} -> s3) (\s@UpdateClassificationScope' {} a -> s {s3 = a} :: UpdateClassificationScope)

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
updateClassificationScope_id :: Lens.Lens' UpdateClassificationScope Prelude.Text
updateClassificationScope_id = Lens.lens (\UpdateClassificationScope' {id} -> id) (\s@UpdateClassificationScope' {} a -> s {id = a} :: UpdateClassificationScope)

instance Core.AWSRequest UpdateClassificationScope where
  type
    AWSResponse UpdateClassificationScope =
      UpdateClassificationScopeResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateClassificationScopeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateClassificationScope where
  hashWithSalt _salt UpdateClassificationScope' {..} =
    _salt `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateClassificationScope where
  rnf UpdateClassificationScope' {..} =
    Prelude.rnf s3 `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateClassificationScope where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateClassificationScope where
  toJSON UpdateClassificationScope' {..} =
    Data.object
      (Prelude.catMaybes [("s3" Data..=) Prelude.<$> s3])

instance Data.ToPath UpdateClassificationScope where
  toPath UpdateClassificationScope' {..} =
    Prelude.mconcat
      ["/classification-scopes/", Data.toBS id]

instance Data.ToQuery UpdateClassificationScope where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateClassificationScopeResponse' smart constructor.
data UpdateClassificationScopeResponse = UpdateClassificationScopeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateClassificationScopeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateClassificationScopeResponse_httpStatus' - The response's http status code.
newUpdateClassificationScopeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateClassificationScopeResponse
newUpdateClassificationScopeResponse pHttpStatus_ =
  UpdateClassificationScopeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateClassificationScopeResponse_httpStatus :: Lens.Lens' UpdateClassificationScopeResponse Prelude.Int
updateClassificationScopeResponse_httpStatus = Lens.lens (\UpdateClassificationScopeResponse' {httpStatus} -> httpStatus) (\s@UpdateClassificationScopeResponse' {} a -> s {httpStatus = a} :: UpdateClassificationScopeResponse)

instance
  Prelude.NFData
    UpdateClassificationScopeResponse
  where
  rnf UpdateClassificationScopeResponse' {..} =
    Prelude.rnf httpStatus
