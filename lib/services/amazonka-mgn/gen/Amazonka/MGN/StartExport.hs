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
-- Module      : Amazonka.MGN.StartExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start export.
module Amazonka.MGN.StartExport
  ( -- * Creating a Request
    StartExport (..),
    newStartExport,

    -- * Request Lenses
    startExport_s3BucketOwner,
    startExport_s3Bucket,
    startExport_s3Key,

    -- * Destructuring the Response
    StartExportResponse (..),
    newStartExportResponse,

    -- * Response Lenses
    startExportResponse_exportTask,
    startExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Start export request.
--
-- /See:/ 'newStartExport' smart constructor.
data StartExport = StartExport'
  { -- | Start export request s3 bucket owner.
    s3BucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Start export request s3 bucket.
    s3Bucket :: Prelude.Text,
    -- | Start export request s3key.
    s3Key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketOwner', 'startExport_s3BucketOwner' - Start export request s3 bucket owner.
--
-- 's3Bucket', 'startExport_s3Bucket' - Start export request s3 bucket.
--
-- 's3Key', 'startExport_s3Key' - Start export request s3key.
newStartExport ::
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Key'
  Prelude.Text ->
  StartExport
newStartExport pS3Bucket_ pS3Key_ =
  StartExport'
    { s3BucketOwner = Prelude.Nothing,
      s3Bucket = pS3Bucket_,
      s3Key = pS3Key_
    }

-- | Start export request s3 bucket owner.
startExport_s3BucketOwner :: Lens.Lens' StartExport (Prelude.Maybe Prelude.Text)
startExport_s3BucketOwner = Lens.lens (\StartExport' {s3BucketOwner} -> s3BucketOwner) (\s@StartExport' {} a -> s {s3BucketOwner = a} :: StartExport)

-- | Start export request s3 bucket.
startExport_s3Bucket :: Lens.Lens' StartExport Prelude.Text
startExport_s3Bucket = Lens.lens (\StartExport' {s3Bucket} -> s3Bucket) (\s@StartExport' {} a -> s {s3Bucket = a} :: StartExport)

-- | Start export request s3key.
startExport_s3Key :: Lens.Lens' StartExport Prelude.Text
startExport_s3Key = Lens.lens (\StartExport' {s3Key} -> s3Key) (\s@StartExport' {} a -> s {s3Key = a} :: StartExport)

instance Core.AWSRequest StartExport where
  type AWSResponse StartExport = StartExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportResponse'
            Prelude.<$> (x Data..?> "exportTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExport where
  hashWithSalt _salt StartExport' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketOwner
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData StartExport where
  rnf StartExport' {..} =
    Prelude.rnf s3BucketOwner
      `Prelude.seq` Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToHeaders StartExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExport where
  toJSON StartExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("s3BucketOwner" Data..=) Prelude.<$> s3BucketOwner,
            Prelude.Just ("s3Bucket" Data..= s3Bucket),
            Prelude.Just ("s3Key" Data..= s3Key)
          ]
      )

instance Data.ToPath StartExport where
  toPath = Prelude.const "/StartExport"

instance Data.ToQuery StartExport where
  toQuery = Prelude.const Prelude.mempty

-- | Start export response.
--
-- /See:/ 'newStartExportResponse' smart constructor.
data StartExportResponse = StartExportResponse'
  { -- | Start export response export task.
    exportTask :: Prelude.Maybe ExportTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportTask', 'startExportResponse_exportTask' - Start export response export task.
--
-- 'httpStatus', 'startExportResponse_httpStatus' - The response's http status code.
newStartExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExportResponse
newStartExportResponse pHttpStatus_ =
  StartExportResponse'
    { exportTask = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Start export response export task.
startExportResponse_exportTask :: Lens.Lens' StartExportResponse (Prelude.Maybe ExportTask)
startExportResponse_exportTask = Lens.lens (\StartExportResponse' {exportTask} -> exportTask) (\s@StartExportResponse' {} a -> s {exportTask = a} :: StartExportResponse)

-- | The response's http status code.
startExportResponse_httpStatus :: Lens.Lens' StartExportResponse Prelude.Int
startExportResponse_httpStatus = Lens.lens (\StartExportResponse' {httpStatus} -> httpStatus) (\s@StartExportResponse' {} a -> s {httpStatus = a} :: StartExportResponse)

instance Prelude.NFData StartExportResponse where
  rnf StartExportResponse' {..} =
    Prelude.rnf exportTask
      `Prelude.seq` Prelude.rnf httpStatus
