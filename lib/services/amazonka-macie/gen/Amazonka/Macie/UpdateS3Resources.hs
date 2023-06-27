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
-- Module      : Amazonka.Macie.UpdateS3Resources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- (Discontinued) Updates the classification types for the specified S3
-- resources. If @memberAccountId@ isn\'t specified, the action updates the
-- classification types of the S3 resources associated with Amazon Macie
-- Classic for the current Macie Classic administrator account. If
-- @memberAccountId@ is specified, the action updates the classification
-- types of the S3 resources associated with Macie Classic for the
-- specified member account.
module Amazonka.Macie.UpdateS3Resources
  ( -- * Creating a Request
    UpdateS3Resources (..),
    newUpdateS3Resources,

    -- * Request Lenses
    updateS3Resources_memberAccountId,
    updateS3Resources_s3ResourcesUpdate,

    -- * Destructuring the Response
    UpdateS3ResourcesResponse (..),
    newUpdateS3ResourcesResponse,

    -- * Response Lenses
    updateS3ResourcesResponse_failedS3Resources,
    updateS3ResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateS3Resources' smart constructor.
data UpdateS3Resources = UpdateS3Resources'
  { -- | (Discontinued) The Amazon Web Services account ID of the Amazon Macie
    -- Classic member account whose S3 resources\' classification types you
    -- want to update.
    memberAccountId :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The S3 resources whose classification types you want to
    -- update.
    s3ResourcesUpdate :: [S3ResourceClassificationUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateS3Resources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'updateS3Resources_memberAccountId' - (Discontinued) The Amazon Web Services account ID of the Amazon Macie
-- Classic member account whose S3 resources\' classification types you
-- want to update.
--
-- 's3ResourcesUpdate', 'updateS3Resources_s3ResourcesUpdate' - (Discontinued) The S3 resources whose classification types you want to
-- update.
newUpdateS3Resources ::
  UpdateS3Resources
newUpdateS3Resources =
  UpdateS3Resources'
    { memberAccountId =
        Prelude.Nothing,
      s3ResourcesUpdate = Prelude.mempty
    }

-- | (Discontinued) The Amazon Web Services account ID of the Amazon Macie
-- Classic member account whose S3 resources\' classification types you
-- want to update.
updateS3Resources_memberAccountId :: Lens.Lens' UpdateS3Resources (Prelude.Maybe Prelude.Text)
updateS3Resources_memberAccountId = Lens.lens (\UpdateS3Resources' {memberAccountId} -> memberAccountId) (\s@UpdateS3Resources' {} a -> s {memberAccountId = a} :: UpdateS3Resources)

-- | (Discontinued) The S3 resources whose classification types you want to
-- update.
updateS3Resources_s3ResourcesUpdate :: Lens.Lens' UpdateS3Resources [S3ResourceClassificationUpdate]
updateS3Resources_s3ResourcesUpdate = Lens.lens (\UpdateS3Resources' {s3ResourcesUpdate} -> s3ResourcesUpdate) (\s@UpdateS3Resources' {} a -> s {s3ResourcesUpdate = a} :: UpdateS3Resources) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateS3Resources where
  type
    AWSResponse UpdateS3Resources =
      UpdateS3ResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateS3ResourcesResponse'
            Prelude.<$> ( x
                            Data..?> "failedS3Resources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateS3Resources where
  hashWithSalt _salt UpdateS3Resources' {..} =
    _salt
      `Prelude.hashWithSalt` memberAccountId
      `Prelude.hashWithSalt` s3ResourcesUpdate

instance Prelude.NFData UpdateS3Resources where
  rnf UpdateS3Resources' {..} =
    Prelude.rnf memberAccountId
      `Prelude.seq` Prelude.rnf s3ResourcesUpdate

instance Data.ToHeaders UpdateS3Resources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MacieService.UpdateS3Resources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateS3Resources where
  toJSON UpdateS3Resources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("memberAccountId" Data..=)
              Prelude.<$> memberAccountId,
            Prelude.Just
              ("s3ResourcesUpdate" Data..= s3ResourcesUpdate)
          ]
      )

instance Data.ToPath UpdateS3Resources where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateS3Resources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateS3ResourcesResponse' smart constructor.
data UpdateS3ResourcesResponse = UpdateS3ResourcesResponse'
  { -- | (Discontinued) The S3 resources whose classification types can\'t be
    -- updated. An error code and an error message are provided for each failed
    -- item.
    failedS3Resources :: Prelude.Maybe [FailedS3Resource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateS3ResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedS3Resources', 'updateS3ResourcesResponse_failedS3Resources' - (Discontinued) The S3 resources whose classification types can\'t be
-- updated. An error code and an error message are provided for each failed
-- item.
--
-- 'httpStatus', 'updateS3ResourcesResponse_httpStatus' - The response's http status code.
newUpdateS3ResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateS3ResourcesResponse
newUpdateS3ResourcesResponse pHttpStatus_ =
  UpdateS3ResourcesResponse'
    { failedS3Resources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Discontinued) The S3 resources whose classification types can\'t be
-- updated. An error code and an error message are provided for each failed
-- item.
updateS3ResourcesResponse_failedS3Resources :: Lens.Lens' UpdateS3ResourcesResponse (Prelude.Maybe [FailedS3Resource])
updateS3ResourcesResponse_failedS3Resources = Lens.lens (\UpdateS3ResourcesResponse' {failedS3Resources} -> failedS3Resources) (\s@UpdateS3ResourcesResponse' {} a -> s {failedS3Resources = a} :: UpdateS3ResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateS3ResourcesResponse_httpStatus :: Lens.Lens' UpdateS3ResourcesResponse Prelude.Int
updateS3ResourcesResponse_httpStatus = Lens.lens (\UpdateS3ResourcesResponse' {httpStatus} -> httpStatus) (\s@UpdateS3ResourcesResponse' {} a -> s {httpStatus = a} :: UpdateS3ResourcesResponse)

instance Prelude.NFData UpdateS3ResourcesResponse where
  rnf UpdateS3ResourcesResponse' {..} =
    Prelude.rnf failedS3Resources
      `Prelude.seq` Prelude.rnf httpStatus
