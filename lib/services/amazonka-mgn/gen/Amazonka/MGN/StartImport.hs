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
-- Module      : Amazonka.MGN.StartImport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start import.
module Amazonka.MGN.StartImport
  ( -- * Creating a Request
    StartImport (..),
    newStartImport,

    -- * Request Lenses
    startImport_clientToken,
    startImport_s3BucketSource,

    -- * Destructuring the Response
    StartImportResponse (..),
    newStartImportResponse,

    -- * Response Lenses
    startImportResponse_importTask,
    startImportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Start import request.
--
-- /See:/ 'newStartImport' smart constructor.
data StartImport = StartImport'
  { -- | Start import request client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Start import request s3 bucket source.
    s3BucketSource :: S3BucketSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startImport_clientToken' - Start import request client token.
--
-- 's3BucketSource', 'startImport_s3BucketSource' - Start import request s3 bucket source.
newStartImport ::
  -- | 's3BucketSource'
  S3BucketSource ->
  StartImport
newStartImport pS3BucketSource_ =
  StartImport'
    { clientToken = Prelude.Nothing,
      s3BucketSource = pS3BucketSource_
    }

-- | Start import request client token.
startImport_clientToken :: Lens.Lens' StartImport (Prelude.Maybe Prelude.Text)
startImport_clientToken = Lens.lens (\StartImport' {clientToken} -> clientToken) (\s@StartImport' {} a -> s {clientToken = a} :: StartImport)

-- | Start import request s3 bucket source.
startImport_s3BucketSource :: Lens.Lens' StartImport S3BucketSource
startImport_s3BucketSource = Lens.lens (\StartImport' {s3BucketSource} -> s3BucketSource) (\s@StartImport' {} a -> s {s3BucketSource = a} :: StartImport)

instance Core.AWSRequest StartImport where
  type AWSResponse StartImport = StartImportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartImportResponse'
            Prelude.<$> (x Data..?> "importTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartImport where
  hashWithSalt _salt StartImport' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` s3BucketSource

instance Prelude.NFData StartImport where
  rnf StartImport' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf s3BucketSource

instance Data.ToHeaders StartImport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartImport where
  toJSON StartImport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("s3BucketSource" Data..= s3BucketSource)
          ]
      )

instance Data.ToPath StartImport where
  toPath = Prelude.const "/StartImport"

instance Data.ToQuery StartImport where
  toQuery = Prelude.const Prelude.mempty

-- | Start import response.
--
-- /See:/ 'newStartImportResponse' smart constructor.
data StartImportResponse = StartImportResponse'
  { -- | Start import response import task.
    importTask :: Prelude.Maybe ImportTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartImportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'importTask', 'startImportResponse_importTask' - Start import response import task.
--
-- 'httpStatus', 'startImportResponse_httpStatus' - The response's http status code.
newStartImportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartImportResponse
newStartImportResponse pHttpStatus_ =
  StartImportResponse'
    { importTask = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Start import response import task.
startImportResponse_importTask :: Lens.Lens' StartImportResponse (Prelude.Maybe ImportTask)
startImportResponse_importTask = Lens.lens (\StartImportResponse' {importTask} -> importTask) (\s@StartImportResponse' {} a -> s {importTask = a} :: StartImportResponse)

-- | The response's http status code.
startImportResponse_httpStatus :: Lens.Lens' StartImportResponse Prelude.Int
startImportResponse_httpStatus = Lens.lens (\StartImportResponse' {httpStatus} -> httpStatus) (\s@StartImportResponse' {} a -> s {httpStatus = a} :: StartImportResponse)

instance Prelude.NFData StartImportResponse where
  rnf StartImportResponse' {..} =
    Prelude.rnf importTask
      `Prelude.seq` Prelude.rnf httpStatus
