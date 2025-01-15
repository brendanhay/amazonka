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
-- Module      : Amazonka.MacieV2.UpdateResourceProfileDetections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the sensitivity scoring settings for an S3 bucket.
module Amazonka.MacieV2.UpdateResourceProfileDetections
  ( -- * Creating a Request
    UpdateResourceProfileDetections (..),
    newUpdateResourceProfileDetections,

    -- * Request Lenses
    updateResourceProfileDetections_suppressDataIdentifiers,
    updateResourceProfileDetections_resourceArn,

    -- * Destructuring the Response
    UpdateResourceProfileDetectionsResponse (..),
    newUpdateResourceProfileDetectionsResponse,

    -- * Response Lenses
    updateResourceProfileDetectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourceProfileDetections' smart constructor.
data UpdateResourceProfileDetections = UpdateResourceProfileDetections'
  { -- | An array of objects, one for each custom data identifier or managed data
    -- identifier that detected the type of sensitive data to start excluding
    -- or including in the bucket\'s score. To start including all sensitive
    -- data types in the score, don\'t specify any values for this array.
    suppressDataIdentifiers :: Prelude.Maybe [SuppressDataIdentifier],
    -- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
    -- to.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceProfileDetections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressDataIdentifiers', 'updateResourceProfileDetections_suppressDataIdentifiers' - An array of objects, one for each custom data identifier or managed data
-- identifier that detected the type of sensitive data to start excluding
-- or including in the bucket\'s score. To start including all sensitive
-- data types in the score, don\'t specify any values for this array.
--
-- 'resourceArn', 'updateResourceProfileDetections_resourceArn' - The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
newUpdateResourceProfileDetections ::
  -- | 'resourceArn'
  Prelude.Text ->
  UpdateResourceProfileDetections
newUpdateResourceProfileDetections pResourceArn_ =
  UpdateResourceProfileDetections'
    { suppressDataIdentifiers =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | An array of objects, one for each custom data identifier or managed data
-- identifier that detected the type of sensitive data to start excluding
-- or including in the bucket\'s score. To start including all sensitive
-- data types in the score, don\'t specify any values for this array.
updateResourceProfileDetections_suppressDataIdentifiers :: Lens.Lens' UpdateResourceProfileDetections (Prelude.Maybe [SuppressDataIdentifier])
updateResourceProfileDetections_suppressDataIdentifiers = Lens.lens (\UpdateResourceProfileDetections' {suppressDataIdentifiers} -> suppressDataIdentifiers) (\s@UpdateResourceProfileDetections' {} a -> s {suppressDataIdentifiers = a} :: UpdateResourceProfileDetections) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the S3 bucket that the request applies
-- to.
updateResourceProfileDetections_resourceArn :: Lens.Lens' UpdateResourceProfileDetections Prelude.Text
updateResourceProfileDetections_resourceArn = Lens.lens (\UpdateResourceProfileDetections' {resourceArn} -> resourceArn) (\s@UpdateResourceProfileDetections' {} a -> s {resourceArn = a} :: UpdateResourceProfileDetections)

instance
  Core.AWSRequest
    UpdateResourceProfileDetections
  where
  type
    AWSResponse UpdateResourceProfileDetections =
      UpdateResourceProfileDetectionsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourceProfileDetectionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateResourceProfileDetections
  where
  hashWithSalt
    _salt
    UpdateResourceProfileDetections' {..} =
      _salt
        `Prelude.hashWithSalt` suppressDataIdentifiers
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    UpdateResourceProfileDetections
  where
  rnf UpdateResourceProfileDetections' {..} =
    Prelude.rnf suppressDataIdentifiers `Prelude.seq`
      Prelude.rnf resourceArn

instance
  Data.ToHeaders
    UpdateResourceProfileDetections
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResourceProfileDetections where
  toJSON UpdateResourceProfileDetections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("suppressDataIdentifiers" Data..=)
              Prelude.<$> suppressDataIdentifiers
          ]
      )

instance Data.ToPath UpdateResourceProfileDetections where
  toPath =
    Prelude.const "/resource-profiles/detections"

instance Data.ToQuery UpdateResourceProfileDetections where
  toQuery UpdateResourceProfileDetections' {..} =
    Prelude.mconcat ["resourceArn" Data.=: resourceArn]

-- | /See:/ 'newUpdateResourceProfileDetectionsResponse' smart constructor.
data UpdateResourceProfileDetectionsResponse = UpdateResourceProfileDetectionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceProfileDetectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourceProfileDetectionsResponse_httpStatus' - The response's http status code.
newUpdateResourceProfileDetectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceProfileDetectionsResponse
newUpdateResourceProfileDetectionsResponse
  pHttpStatus_ =
    UpdateResourceProfileDetectionsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateResourceProfileDetectionsResponse_httpStatus :: Lens.Lens' UpdateResourceProfileDetectionsResponse Prelude.Int
updateResourceProfileDetectionsResponse_httpStatus = Lens.lens (\UpdateResourceProfileDetectionsResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceProfileDetectionsResponse' {} a -> s {httpStatus = a} :: UpdateResourceProfileDetectionsResponse)

instance
  Prelude.NFData
    UpdateResourceProfileDetectionsResponse
  where
  rnf UpdateResourceProfileDetectionsResponse' {..} =
    Prelude.rnf httpStatus
