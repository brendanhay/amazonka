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
-- Module      : Amazonka.DevOpsGuru.DeleteInsight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the insight along with the associated anomalies, events and
-- recommendations.
module Amazonka.DevOpsGuru.DeleteInsight
  ( -- * Creating a Request
    DeleteInsight (..),
    newDeleteInsight,

    -- * Request Lenses
    deleteInsight_id,

    -- * Destructuring the Response
    DeleteInsightResponse (..),
    newDeleteInsightResponse,

    -- * Response Lenses
    deleteInsightResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInsight' smart constructor.
data DeleteInsight = DeleteInsight'
  { -- | The ID of the insight.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteInsight_id' - The ID of the insight.
newDeleteInsight ::
  -- | 'id'
  Prelude.Text ->
  DeleteInsight
newDeleteInsight pId_ = DeleteInsight' {id = pId_}

-- | The ID of the insight.
deleteInsight_id :: Lens.Lens' DeleteInsight Prelude.Text
deleteInsight_id = Lens.lens (\DeleteInsight' {id} -> id) (\s@DeleteInsight' {} a -> s {id = a} :: DeleteInsight)

instance Core.AWSRequest DeleteInsight where
  type
    AWSResponse DeleteInsight =
      DeleteInsightResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteInsightResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteInsight where
  hashWithSalt _salt DeleteInsight' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteInsight where
  rnf DeleteInsight' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteInsight where
  toPath DeleteInsight' {..} =
    Prelude.mconcat ["/insights/", Core.toBS id]

instance Core.ToQuery DeleteInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteInsightResponse' smart constructor.
data DeleteInsightResponse = DeleteInsightResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteInsightResponse_httpStatus' - The response's http status code.
newDeleteInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInsightResponse
newDeleteInsightResponse pHttpStatus_ =
  DeleteInsightResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteInsightResponse_httpStatus :: Lens.Lens' DeleteInsightResponse Prelude.Int
deleteInsightResponse_httpStatus = Lens.lens (\DeleteInsightResponse' {httpStatus} -> httpStatus) (\s@DeleteInsightResponse' {} a -> s {httpStatus = a} :: DeleteInsightResponse)

instance Prelude.NFData DeleteInsightResponse where
  rnf DeleteInsightResponse' {..} =
    Prelude.rnf httpStatus
