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
-- Module      : Amazonka.Pinpoint.DeleteSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment from an application.
module Amazonka.Pinpoint.DeleteSegment
  ( -- * Creating a Request
    DeleteSegment (..),
    newDeleteSegment,

    -- * Request Lenses
    deleteSegment_segmentId,
    deleteSegment_applicationId,

    -- * Destructuring the Response
    DeleteSegmentResponse (..),
    newDeleteSegmentResponse,

    -- * Response Lenses
    deleteSegmentResponse_httpStatus,
    deleteSegmentResponse_segmentResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { -- | The unique identifier for the segment.
    segmentId :: Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentId', 'deleteSegment_segmentId' - The unique identifier for the segment.
--
-- 'applicationId', 'deleteSegment_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newDeleteSegment ::
  -- | 'segmentId'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  DeleteSegment
newDeleteSegment pSegmentId_ pApplicationId_ =
  DeleteSegment'
    { segmentId = pSegmentId_,
      applicationId = pApplicationId_
    }

-- | The unique identifier for the segment.
deleteSegment_segmentId :: Lens.Lens' DeleteSegment Prelude.Text
deleteSegment_segmentId = Lens.lens (\DeleteSegment' {segmentId} -> segmentId) (\s@DeleteSegment' {} a -> s {segmentId = a} :: DeleteSegment)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteSegment_applicationId :: Lens.Lens' DeleteSegment Prelude.Text
deleteSegment_applicationId = Lens.lens (\DeleteSegment' {applicationId} -> applicationId) (\s@DeleteSegment' {} a -> s {applicationId = a} :: DeleteSegment)

instance Core.AWSRequest DeleteSegment where
  type
    AWSResponse DeleteSegment =
      DeleteSegmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable DeleteSegment where
  hashWithSalt _salt DeleteSegment' {..} =
    _salt `Prelude.hashWithSalt` segmentId
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteSegment where
  rnf DeleteSegment' {..} =
    Prelude.rnf segmentId
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders DeleteSegment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSegment where
  toPath DeleteSegment' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/segments/",
        Data.toBS segmentId
      ]

instance Data.ToQuery DeleteSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    segmentResponse :: SegmentResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSegmentResponse_httpStatus' - The response's http status code.
--
-- 'segmentResponse', 'deleteSegmentResponse_segmentResponse' - Undocumented member.
newDeleteSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'segmentResponse'
  SegmentResponse ->
  DeleteSegmentResponse
newDeleteSegmentResponse
  pHttpStatus_
  pSegmentResponse_ =
    DeleteSegmentResponse'
      { httpStatus = pHttpStatus_,
        segmentResponse = pSegmentResponse_
      }

-- | The response's http status code.
deleteSegmentResponse_httpStatus :: Lens.Lens' DeleteSegmentResponse Prelude.Int
deleteSegmentResponse_httpStatus = Lens.lens (\DeleteSegmentResponse' {httpStatus} -> httpStatus) (\s@DeleteSegmentResponse' {} a -> s {httpStatus = a} :: DeleteSegmentResponse)

-- | Undocumented member.
deleteSegmentResponse_segmentResponse :: Lens.Lens' DeleteSegmentResponse SegmentResponse
deleteSegmentResponse_segmentResponse = Lens.lens (\DeleteSegmentResponse' {segmentResponse} -> segmentResponse) (\s@DeleteSegmentResponse' {} a -> s {segmentResponse = a} :: DeleteSegmentResponse)

instance Prelude.NFData DeleteSegmentResponse where
  rnf DeleteSegmentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf segmentResponse
