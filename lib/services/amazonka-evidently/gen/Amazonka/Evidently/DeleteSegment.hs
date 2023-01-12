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
-- Module      : Amazonka.Evidently.DeleteSegment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a segment. You can\'t delete a segment that is being used in a
-- launch or experiment, even if that launch or experiment is not currently
-- running.
module Amazonka.Evidently.DeleteSegment
  ( -- * Creating a Request
    DeleteSegment (..),
    newDeleteSegment,

    -- * Request Lenses
    deleteSegment_segment,

    -- * Destructuring the Response
    DeleteSegmentResponse (..),
    newDeleteSegmentResponse,

    -- * Response Lenses
    deleteSegmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSegment' smart constructor.
data DeleteSegment = DeleteSegment'
  { -- | Specifies the segment to delete.
    segment :: Prelude.Text
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
-- 'segment', 'deleteSegment_segment' - Specifies the segment to delete.
newDeleteSegment ::
  -- | 'segment'
  Prelude.Text ->
  DeleteSegment
newDeleteSegment pSegment_ =
  DeleteSegment' {segment = pSegment_}

-- | Specifies the segment to delete.
deleteSegment_segment :: Lens.Lens' DeleteSegment Prelude.Text
deleteSegment_segment = Lens.lens (\DeleteSegment' {segment} -> segment) (\s@DeleteSegment' {} a -> s {segment = a} :: DeleteSegment)

instance Core.AWSRequest DeleteSegment where
  type
    AWSResponse DeleteSegment =
      DeleteSegmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSegmentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSegment where
  hashWithSalt _salt DeleteSegment' {..} =
    _salt `Prelude.hashWithSalt` segment

instance Prelude.NFData DeleteSegment where
  rnf DeleteSegment' {..} = Prelude.rnf segment

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
    Prelude.mconcat ["/segments/", Data.toBS segment]

instance Data.ToQuery DeleteSegment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSegmentResponse' smart constructor.
data DeleteSegmentResponse = DeleteSegmentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
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
newDeleteSegmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSegmentResponse
newDeleteSegmentResponse pHttpStatus_ =
  DeleteSegmentResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteSegmentResponse_httpStatus :: Lens.Lens' DeleteSegmentResponse Prelude.Int
deleteSegmentResponse_httpStatus = Lens.lens (\DeleteSegmentResponse' {httpStatus} -> httpStatus) (\s@DeleteSegmentResponse' {} a -> s {httpStatus = a} :: DeleteSegmentResponse)

instance Prelude.NFData DeleteSegmentResponse where
  rnf DeleteSegmentResponse' {..} =
    Prelude.rnf httpStatus
