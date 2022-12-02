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
-- Module      : Amazonka.DeviceFarm.DeleteRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the run, given the run ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Amazonka.DeviceFarm.DeleteRun
  ( -- * Creating a Request
    DeleteRun (..),
    newDeleteRun,

    -- * Request Lenses
    deleteRun_arn,

    -- * Destructuring the Response
    DeleteRunResponse (..),
    newDeleteRunResponse,

    -- * Response Lenses
    deleteRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the delete run operation.
--
-- /See:/ 'newDeleteRun' smart constructor.
data DeleteRun = DeleteRun'
  { -- | The Amazon Resource Name (ARN) for the run to delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRun_arn' - The Amazon Resource Name (ARN) for the run to delete.
newDeleteRun ::
  -- | 'arn'
  Prelude.Text ->
  DeleteRun
newDeleteRun pArn_ = DeleteRun' {arn = pArn_}

-- | The Amazon Resource Name (ARN) for the run to delete.
deleteRun_arn :: Lens.Lens' DeleteRun Prelude.Text
deleteRun_arn = Lens.lens (\DeleteRun' {arn} -> arn) (\s@DeleteRun' {} a -> s {arn = a} :: DeleteRun)

instance Core.AWSRequest DeleteRun where
  type AWSResponse DeleteRun = DeleteRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRun where
  hashWithSalt _salt DeleteRun' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteRun where
  rnf DeleteRun' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteRun" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRun where
  toJSON DeleteRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteRun where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRun where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a delete run request.
--
-- /See:/ 'newDeleteRunResponse' smart constructor.
data DeleteRunResponse = DeleteRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRunResponse_httpStatus' - The response's http status code.
newDeleteRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRunResponse
newDeleteRunResponse pHttpStatus_ =
  DeleteRunResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRunResponse_httpStatus :: Lens.Lens' DeleteRunResponse Prelude.Int
deleteRunResponse_httpStatus = Lens.lens (\DeleteRunResponse' {httpStatus} -> httpStatus) (\s@DeleteRunResponse' {} a -> s {httpStatus = a} :: DeleteRunResponse)

instance Prelude.NFData DeleteRunResponse where
  rnf DeleteRunResponse' {..} = Prelude.rnf httpStatus
