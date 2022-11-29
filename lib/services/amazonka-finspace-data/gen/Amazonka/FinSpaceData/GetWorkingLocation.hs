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
-- Module      : Amazonka.FinSpaceData.GetWorkingLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A temporary Amazon S3 location, where you can copy your files from a
-- source location to stage or use as a scratch space in FinSpace notebook.
module Amazonka.FinSpaceData.GetWorkingLocation
  ( -- * Creating a Request
    GetWorkingLocation (..),
    newGetWorkingLocation,

    -- * Request Lenses
    getWorkingLocation_locationType,

    -- * Destructuring the Response
    GetWorkingLocationResponse (..),
    newGetWorkingLocationResponse,

    -- * Response Lenses
    getWorkingLocationResponse_s3Bucket,
    getWorkingLocationResponse_s3Path,
    getWorkingLocationResponse_s3Uri,
    getWorkingLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkingLocation' smart constructor.
data GetWorkingLocation = GetWorkingLocation'
  { -- | Specify the type of the working location.
    --
    -- -   @SAGEMAKER@ – Use the Amazon S3 location as a temporary location to
    --     store data content when working with FinSpace Notebooks that run on
    --     SageMaker studio.
    --
    -- -   @INGESTION@ – Use the Amazon S3 location as a staging location to
    --     copy your data content and then use the location with the Changeset
    --     creation operation.
    locationType :: Prelude.Maybe LocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkingLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationType', 'getWorkingLocation_locationType' - Specify the type of the working location.
--
-- -   @SAGEMAKER@ – Use the Amazon S3 location as a temporary location to
--     store data content when working with FinSpace Notebooks that run on
--     SageMaker studio.
--
-- -   @INGESTION@ – Use the Amazon S3 location as a staging location to
--     copy your data content and then use the location with the Changeset
--     creation operation.
newGetWorkingLocation ::
  GetWorkingLocation
newGetWorkingLocation =
  GetWorkingLocation' {locationType = Prelude.Nothing}

-- | Specify the type of the working location.
--
-- -   @SAGEMAKER@ – Use the Amazon S3 location as a temporary location to
--     store data content when working with FinSpace Notebooks that run on
--     SageMaker studio.
--
-- -   @INGESTION@ – Use the Amazon S3 location as a staging location to
--     copy your data content and then use the location with the Changeset
--     creation operation.
getWorkingLocation_locationType :: Lens.Lens' GetWorkingLocation (Prelude.Maybe LocationType)
getWorkingLocation_locationType = Lens.lens (\GetWorkingLocation' {locationType} -> locationType) (\s@GetWorkingLocation' {} a -> s {locationType = a} :: GetWorkingLocation)

instance Core.AWSRequest GetWorkingLocation where
  type
    AWSResponse GetWorkingLocation =
      GetWorkingLocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkingLocationResponse'
            Prelude.<$> (x Core..?> "s3Bucket")
            Prelude.<*> (x Core..?> "s3Path")
            Prelude.<*> (x Core..?> "s3Uri")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkingLocation where
  hashWithSalt _salt GetWorkingLocation' {..} =
    _salt `Prelude.hashWithSalt` locationType

instance Prelude.NFData GetWorkingLocation where
  rnf GetWorkingLocation' {..} =
    Prelude.rnf locationType

instance Core.ToHeaders GetWorkingLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWorkingLocation where
  toJSON GetWorkingLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [("locationType" Core..=) Prelude.<$> locationType]
      )

instance Core.ToPath GetWorkingLocation where
  toPath = Prelude.const "/workingLocationV1"

instance Core.ToQuery GetWorkingLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkingLocationResponse' smart constructor.
data GetWorkingLocationResponse = GetWorkingLocationResponse'
  { -- | Returns the Amazon S3 bucket name for the working location.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | Returns the Amazon S3 Path for the working location.
    s3Path :: Prelude.Maybe Prelude.Text,
    -- | Returns the Amazon S3 URI for the working location.
    s3Uri :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkingLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'getWorkingLocationResponse_s3Bucket' - Returns the Amazon S3 bucket name for the working location.
--
-- 's3Path', 'getWorkingLocationResponse_s3Path' - Returns the Amazon S3 Path for the working location.
--
-- 's3Uri', 'getWorkingLocationResponse_s3Uri' - Returns the Amazon S3 URI for the working location.
--
-- 'httpStatus', 'getWorkingLocationResponse_httpStatus' - The response's http status code.
newGetWorkingLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkingLocationResponse
newGetWorkingLocationResponse pHttpStatus_ =
  GetWorkingLocationResponse'
    { s3Bucket =
        Prelude.Nothing,
      s3Path = Prelude.Nothing,
      s3Uri = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the Amazon S3 bucket name for the working location.
getWorkingLocationResponse_s3Bucket :: Lens.Lens' GetWorkingLocationResponse (Prelude.Maybe Prelude.Text)
getWorkingLocationResponse_s3Bucket = Lens.lens (\GetWorkingLocationResponse' {s3Bucket} -> s3Bucket) (\s@GetWorkingLocationResponse' {} a -> s {s3Bucket = a} :: GetWorkingLocationResponse)

-- | Returns the Amazon S3 Path for the working location.
getWorkingLocationResponse_s3Path :: Lens.Lens' GetWorkingLocationResponse (Prelude.Maybe Prelude.Text)
getWorkingLocationResponse_s3Path = Lens.lens (\GetWorkingLocationResponse' {s3Path} -> s3Path) (\s@GetWorkingLocationResponse' {} a -> s {s3Path = a} :: GetWorkingLocationResponse)

-- | Returns the Amazon S3 URI for the working location.
getWorkingLocationResponse_s3Uri :: Lens.Lens' GetWorkingLocationResponse (Prelude.Maybe Prelude.Text)
getWorkingLocationResponse_s3Uri = Lens.lens (\GetWorkingLocationResponse' {s3Uri} -> s3Uri) (\s@GetWorkingLocationResponse' {} a -> s {s3Uri = a} :: GetWorkingLocationResponse)

-- | The response's http status code.
getWorkingLocationResponse_httpStatus :: Lens.Lens' GetWorkingLocationResponse Prelude.Int
getWorkingLocationResponse_httpStatus = Lens.lens (\GetWorkingLocationResponse' {httpStatus} -> httpStatus) (\s@GetWorkingLocationResponse' {} a -> s {httpStatus = a} :: GetWorkingLocationResponse)

instance Prelude.NFData GetWorkingLocationResponse where
  rnf GetWorkingLocationResponse' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Path
      `Prelude.seq` Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf httpStatus
