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
-- Module      : Amazonka.Pinpoint.CreateSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new segment for an application or updates the configuration,
-- dimension, and other settings for an existing segment that\'s associated
-- with an application.
module Amazonka.Pinpoint.CreateSegment
  ( -- * Creating a Request
    CreateSegment (..),
    newCreateSegment,

    -- * Request Lenses
    createSegment_applicationId,
    createSegment_writeSegmentRequest,

    -- * Destructuring the Response
    CreateSegmentResponse (..),
    newCreateSegmentResponse,

    -- * Response Lenses
    createSegmentResponse_httpStatus,
    createSegmentResponse_segmentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSegment' smart constructor.
data CreateSegment = CreateSegment'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeSegmentRequest :: WriteSegmentRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createSegment_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeSegmentRequest', 'createSegment_writeSegmentRequest' - Undocumented member.
newCreateSegment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeSegmentRequest'
  WriteSegmentRequest ->
  CreateSegment
newCreateSegment
  pApplicationId_
  pWriteSegmentRequest_ =
    CreateSegment'
      { applicationId = pApplicationId_,
        writeSegmentRequest = pWriteSegmentRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
createSegment_applicationId :: Lens.Lens' CreateSegment Prelude.Text
createSegment_applicationId = Lens.lens (\CreateSegment' {applicationId} -> applicationId) (\s@CreateSegment' {} a -> s {applicationId = a} :: CreateSegment)

-- | Undocumented member.
createSegment_writeSegmentRequest :: Lens.Lens' CreateSegment WriteSegmentRequest
createSegment_writeSegmentRequest = Lens.lens (\CreateSegment' {writeSegmentRequest} -> writeSegmentRequest) (\s@CreateSegment' {} a -> s {writeSegmentRequest = a} :: CreateSegment)

instance Core.AWSRequest CreateSegment where
  type
    AWSResponse CreateSegment =
      CreateSegmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable CreateSegment where
  hashWithSalt _salt CreateSegment' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` writeSegmentRequest

instance Prelude.NFData CreateSegment where
  rnf CreateSegment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf writeSegmentRequest

instance Core.ToHeaders CreateSegment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSegment where
  toJSON CreateSegment' {..} =
    Core.toJSON writeSegmentRequest

instance Core.ToPath CreateSegment where
  toPath CreateSegment' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/segments"]

instance Core.ToQuery CreateSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSegmentResponse' smart constructor.
data CreateSegmentResponse = CreateSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'createSegmentResponse_segmentResponse' - Undocumented member.
newCreateSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  CreateSegmentResponse
newCreateSegmentResponse
  pHttpStatus_
  pSegmentResponse_ =
    CreateSegmentResponse'
      { httpStatus = pHttpStatus_,
        segmentResponse = pSegmentResponse_
      }

-- | The response's http status code.
createSegmentResponse_httpStatus :: Lens.Lens' CreateSegmentResponse Prelude.Int
createSegmentResponse_httpStatus = Lens.lens (\CreateSegmentResponse' {httpStatus} -> httpStatus) (\s@CreateSegmentResponse' {} a -> s {httpStatus = a} :: CreateSegmentResponse)

-- | Undocumented member.
createSegmentResponse_segmentResponse :: Lens.Lens' CreateSegmentResponse SegmentResponse
createSegmentResponse_segmentResponse = Lens.lens (\CreateSegmentResponse' {segmentResponse} -> segmentResponse) (\s@CreateSegmentResponse' {} a -> s {segmentResponse = a} :: CreateSegmentResponse)

instance Prelude.NFData CreateSegmentResponse where
  rnf CreateSegmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segmentResponse
