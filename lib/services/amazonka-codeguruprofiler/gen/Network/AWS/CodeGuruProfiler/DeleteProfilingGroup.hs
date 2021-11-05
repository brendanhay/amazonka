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
-- Module      : Network.AWS.CodeGuruProfiler.DeleteProfilingGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profiling group.
module Network.AWS.CodeGuruProfiler.DeleteProfilingGroup
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

import Network.AWS.CodeGuruProfiler.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProfilingGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfilingGroup

instance Prelude.NFData DeleteProfilingGroup

instance Core.ToHeaders DeleteProfilingGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteProfilingGroup where
  toPath DeleteProfilingGroup' {..} =
    Prelude.mconcat
      ["/profilingGroups/", Core.toBS profilingGroupName]

instance Core.ToQuery DeleteProfilingGroup where
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

instance Prelude.NFData DeleteProfilingGroupResponse
