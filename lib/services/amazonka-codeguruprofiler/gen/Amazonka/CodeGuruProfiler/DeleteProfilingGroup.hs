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
-- Module      : Amazonka.CodeGuruProfiler.DeleteProfilingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profiling group.
module Amazonka.CodeGuruProfiler.DeleteProfilingGroup
  ( -- * Creating a Request
    DeleteProfilingGroup (..),
    newDeleteProfilingGroup,

    -- * Request Lenses
    deleteProfilingGroup_profilingGroupName,

    -- * Destructuring the Response
    DeleteProfilingGroupResponse (..),
    newDeleteProfilingGroupResponse,

    -- * Response Lenses
    deleteProfilingGroupResponse_httpStatus,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the deleteProfilingGroupRequest.
--
-- /See:/ 'newDeleteProfilingGroup' smart constructor.
data DeleteProfilingGroup = DeleteProfilingGroup'
  { -- | The name of the profiling group to delete.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfilingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilingGroupName', 'deleteProfilingGroup_profilingGroupName' - The name of the profiling group to delete.
newDeleteProfilingGroup ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  DeleteProfilingGroup
newDeleteProfilingGroup pProfilingGroupName_ =
  DeleteProfilingGroup'
    { profilingGroupName =
        pProfilingGroupName_
    }

-- | The name of the profiling group to delete.
deleteProfilingGroup_profilingGroupName :: Lens.Lens' DeleteProfilingGroup Prelude.Text
deleteProfilingGroup_profilingGroupName = Lens.lens (\DeleteProfilingGroup' {profilingGroupName} -> profilingGroupName) (\s@DeleteProfilingGroup' {} a -> s {profilingGroupName = a} :: DeleteProfilingGroup)

instance Core.AWSRequest DeleteProfilingGroup where
  type
    AWSResponse DeleteProfilingGroup =
      DeleteProfilingGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProfilingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfilingGroup where
  hashWithSalt _salt DeleteProfilingGroup' {..} =
    _salt `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData DeleteProfilingGroup where
  rnf DeleteProfilingGroup' {..} =
    Prelude.rnf profilingGroupName

instance Data.ToHeaders DeleteProfilingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProfilingGroup where
  toPath DeleteProfilingGroup' {..} =
    Prelude.mconcat
      ["/profilingGroups/", Data.toBS profilingGroupName]

instance Data.ToQuery DeleteProfilingGroup where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the deleteProfilingGroupResponse.
--
-- /See:/ 'newDeleteProfilingGroupResponse' smart constructor.
data DeleteProfilingGroupResponse = DeleteProfilingGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfilingGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProfilingGroupResponse_httpStatus' - The response's http status code.
newDeleteProfilingGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProfilingGroupResponse
newDeleteProfilingGroupResponse pHttpStatus_ =
  DeleteProfilingGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProfilingGroupResponse_httpStatus :: Lens.Lens' DeleteProfilingGroupResponse Prelude.Int
deleteProfilingGroupResponse_httpStatus = Lens.lens (\DeleteProfilingGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteProfilingGroupResponse' {} a -> s {httpStatus = a} :: DeleteProfilingGroupResponse)

instance Prelude.NFData DeleteProfilingGroupResponse where
  rnf DeleteProfilingGroupResponse' {..} =
    Prelude.rnf httpStatus
