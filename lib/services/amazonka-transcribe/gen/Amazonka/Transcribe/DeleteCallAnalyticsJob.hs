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
-- Module      : Amazonka.Transcribe.DeleteCallAnalyticsJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Call Analytics job. To use this operation, specify the name of
-- the job you want to delete using @CallAnalyticsJobName@. Job names are
-- case sensitive.
module Amazonka.Transcribe.DeleteCallAnalyticsJob
  ( -- * Creating a Request
    DeleteCallAnalyticsJob (..),
    newDeleteCallAnalyticsJob,

    -- * Request Lenses
    deleteCallAnalyticsJob_callAnalyticsJobName,

    -- * Destructuring the Response
    DeleteCallAnalyticsJobResponse (..),
    newDeleteCallAnalyticsJobResponse,

    -- * Response Lenses
    deleteCallAnalyticsJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteCallAnalyticsJob' smart constructor.
data DeleteCallAnalyticsJob = DeleteCallAnalyticsJob'
  { -- | The name of the Call Analytics job you want to delete. Job names are
    -- case sensitive.
    callAnalyticsJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCallAnalyticsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callAnalyticsJobName', 'deleteCallAnalyticsJob_callAnalyticsJobName' - The name of the Call Analytics job you want to delete. Job names are
-- case sensitive.
newDeleteCallAnalyticsJob ::
  -- | 'callAnalyticsJobName'
  Prelude.Text ->
  DeleteCallAnalyticsJob
newDeleteCallAnalyticsJob pCallAnalyticsJobName_ =
  DeleteCallAnalyticsJob'
    { callAnalyticsJobName =
        pCallAnalyticsJobName_
    }

-- | The name of the Call Analytics job you want to delete. Job names are
-- case sensitive.
deleteCallAnalyticsJob_callAnalyticsJobName :: Lens.Lens' DeleteCallAnalyticsJob Prelude.Text
deleteCallAnalyticsJob_callAnalyticsJobName = Lens.lens (\DeleteCallAnalyticsJob' {callAnalyticsJobName} -> callAnalyticsJobName) (\s@DeleteCallAnalyticsJob' {} a -> s {callAnalyticsJobName = a} :: DeleteCallAnalyticsJob)

instance Core.AWSRequest DeleteCallAnalyticsJob where
  type
    AWSResponse DeleteCallAnalyticsJob =
      DeleteCallAnalyticsJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCallAnalyticsJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCallAnalyticsJob where
  hashWithSalt _salt DeleteCallAnalyticsJob' {..} =
    _salt `Prelude.hashWithSalt` callAnalyticsJobName

instance Prelude.NFData DeleteCallAnalyticsJob where
  rnf DeleteCallAnalyticsJob' {..} =
    Prelude.rnf callAnalyticsJobName

instance Data.ToHeaders DeleteCallAnalyticsJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.DeleteCallAnalyticsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteCallAnalyticsJob where
  toJSON DeleteCallAnalyticsJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "CallAnalyticsJobName"
                  Data..= callAnalyticsJobName
              )
          ]
      )

instance Data.ToPath DeleteCallAnalyticsJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteCallAnalyticsJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCallAnalyticsJobResponse' smart constructor.
data DeleteCallAnalyticsJobResponse = DeleteCallAnalyticsJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCallAnalyticsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCallAnalyticsJobResponse_httpStatus' - The response's http status code.
newDeleteCallAnalyticsJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCallAnalyticsJobResponse
newDeleteCallAnalyticsJobResponse pHttpStatus_ =
  DeleteCallAnalyticsJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCallAnalyticsJobResponse_httpStatus :: Lens.Lens' DeleteCallAnalyticsJobResponse Prelude.Int
deleteCallAnalyticsJobResponse_httpStatus = Lens.lens (\DeleteCallAnalyticsJobResponse' {httpStatus} -> httpStatus) (\s@DeleteCallAnalyticsJobResponse' {} a -> s {httpStatus = a} :: DeleteCallAnalyticsJobResponse)

instance
  Prelude.NFData
    DeleteCallAnalyticsJobResponse
  where
  rnf DeleteCallAnalyticsJobResponse' {..} =
    Prelude.rnf httpStatus
