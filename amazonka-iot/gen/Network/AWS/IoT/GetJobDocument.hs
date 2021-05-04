{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.GetJobDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a job document.
module Network.AWS.IoT.GetJobDocument
  ( -- * Creating a Request
    GetJobDocument (..),
    newGetJobDocument,

    -- * Request Lenses
    getJobDocument_jobId,

    -- * Destructuring the Response
    GetJobDocumentResponse (..),
    newGetJobDocumentResponse,

    -- * Response Lenses
    getJobDocumentResponse_document,
    getJobDocumentResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetJobDocument' smart constructor.
data GetJobDocument = GetJobDocument'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetJobDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'getJobDocument_jobId' - The unique identifier you assigned to this job when it was created.
newGetJobDocument ::
  -- | 'jobId'
  Prelude.Text ->
  GetJobDocument
newGetJobDocument pJobId_ =
  GetJobDocument' {jobId = pJobId_}

-- | The unique identifier you assigned to this job when it was created.
getJobDocument_jobId :: Lens.Lens' GetJobDocument Prelude.Text
getJobDocument_jobId = Lens.lens (\GetJobDocument' {jobId} -> jobId) (\s@GetJobDocument' {} a -> s {jobId = a} :: GetJobDocument)

instance Prelude.AWSRequest GetJobDocument where
  type Rs GetJobDocument = GetJobDocumentResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJobDocumentResponse'
            Prelude.<$> (x Prelude..?> "document")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetJobDocument

instance Prelude.NFData GetJobDocument

instance Prelude.ToHeaders GetJobDocument where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetJobDocument where
  toPath GetJobDocument' {..} =
    Prelude.mconcat
      ["/jobs/", Prelude.toBS jobId, "/job-document"]

instance Prelude.ToQuery GetJobDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetJobDocumentResponse' smart constructor.
data GetJobDocumentResponse = GetJobDocumentResponse'
  { -- | The job document content.
    document :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetJobDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'getJobDocumentResponse_document' - The job document content.
--
-- 'httpStatus', 'getJobDocumentResponse_httpStatus' - The response's http status code.
newGetJobDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetJobDocumentResponse
newGetJobDocumentResponse pHttpStatus_ =
  GetJobDocumentResponse'
    { document = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The job document content.
getJobDocumentResponse_document :: Lens.Lens' GetJobDocumentResponse (Prelude.Maybe Prelude.Text)
getJobDocumentResponse_document = Lens.lens (\GetJobDocumentResponse' {document} -> document) (\s@GetJobDocumentResponse' {} a -> s {document = a} :: GetJobDocumentResponse)

-- | The response's http status code.
getJobDocumentResponse_httpStatus :: Lens.Lens' GetJobDocumentResponse Prelude.Int
getJobDocumentResponse_httpStatus = Lens.lens (\GetJobDocumentResponse' {httpStatus} -> httpStatus) (\s@GetJobDocumentResponse' {} a -> s {httpStatus = a} :: GetJobDocumentResponse)

instance Prelude.NFData GetJobDocumentResponse
