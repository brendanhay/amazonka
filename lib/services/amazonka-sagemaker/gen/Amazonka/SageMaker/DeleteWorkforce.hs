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
-- Module      : Amazonka.SageMaker.DeleteWorkforce
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to delete a workforce.
--
-- If you want to create a new workforce in an Amazon Web Services Region
-- where a workforce already exists, use this operation to delete the
-- existing workforce and then use to create a new workforce.
--
-- If a private workforce contains one or more work teams, you must use the
-- operation to delete all work teams before you delete the workforce. If
-- you try to delete a workforce that contains one or more work teams, you
-- will recieve a @ResourceInUse@ error.
module Amazonka.SageMaker.DeleteWorkforce
  ( -- * Creating a Request
    DeleteWorkforce (..),
    newDeleteWorkforce,

    -- * Request Lenses
    deleteWorkforce_workforceName,

    -- * Destructuring the Response
    DeleteWorkforceResponse (..),
    newDeleteWorkforceResponse,

    -- * Response Lenses
    deleteWorkforceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteWorkforce' smart constructor.
data DeleteWorkforce = DeleteWorkforce'
  { -- | The name of the workforce.
    workforceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkforce' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workforceName', 'deleteWorkforce_workforceName' - The name of the workforce.
newDeleteWorkforce ::
  -- | 'workforceName'
  Prelude.Text ->
  DeleteWorkforce
newDeleteWorkforce pWorkforceName_ =
  DeleteWorkforce' {workforceName = pWorkforceName_}

-- | The name of the workforce.
deleteWorkforce_workforceName :: Lens.Lens' DeleteWorkforce Prelude.Text
deleteWorkforce_workforceName = Lens.lens (\DeleteWorkforce' {workforceName} -> workforceName) (\s@DeleteWorkforce' {} a -> s {workforceName = a} :: DeleteWorkforce)

instance Core.AWSRequest DeleteWorkforce where
  type
    AWSResponse DeleteWorkforce =
      DeleteWorkforceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkforceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkforce where
  hashWithSalt _salt DeleteWorkforce' {..} =
    _salt `Prelude.hashWithSalt` workforceName

instance Prelude.NFData DeleteWorkforce where
  rnf DeleteWorkforce' {..} = Prelude.rnf workforceName

instance Data.ToHeaders DeleteWorkforce where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteWorkforce" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWorkforce where
  toJSON DeleteWorkforce' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkforceName" Data..= workforceName)
          ]
      )

instance Data.ToPath DeleteWorkforce where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWorkforce where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkforceResponse' smart constructor.
data DeleteWorkforceResponse = DeleteWorkforceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkforceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkforceResponse_httpStatus' - The response's http status code.
newDeleteWorkforceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkforceResponse
newDeleteWorkforceResponse pHttpStatus_ =
  DeleteWorkforceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkforceResponse_httpStatus :: Lens.Lens' DeleteWorkforceResponse Prelude.Int
deleteWorkforceResponse_httpStatus = Lens.lens (\DeleteWorkforceResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkforceResponse' {} a -> s {httpStatus = a} :: DeleteWorkforceResponse)

instance Prelude.NFData DeleteWorkforceResponse where
  rnf DeleteWorkforceResponse' {..} =
    Prelude.rnf httpStatus
