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
-- Module      : Network.AWS.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup
-- cannot be deleted.
module Network.AWS.Athena.DeleteWorkGroup
  ( -- * Creating a Request
    DeleteWorkGroup (..),
    newDeleteWorkGroup,

    -- * Request Lenses
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,

    -- * Destructuring the Response
    DeleteWorkGroupResponse (..),
    newDeleteWorkGroupResponse,

    -- * Response Lenses
    deleteWorkGroupResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { -- | The option to delete the workgroup and its contents even if the
    -- workgroup contains any named queries or query executions.
    recursiveDeleteOption :: Prelude.Maybe Prelude.Bool,
    -- | The unique name of the workgroup to delete.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recursiveDeleteOption', 'deleteWorkGroup_recursiveDeleteOption' - The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
--
-- 'workGroup', 'deleteWorkGroup_workGroup' - The unique name of the workgroup to delete.
newDeleteWorkGroup ::
  -- | 'workGroup'
  Prelude.Text ->
  DeleteWorkGroup
newDeleteWorkGroup pWorkGroup_ =
  DeleteWorkGroup'
    { recursiveDeleteOption =
        Prelude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
deleteWorkGroup_recursiveDeleteOption :: Lens.Lens' DeleteWorkGroup (Prelude.Maybe Prelude.Bool)
deleteWorkGroup_recursiveDeleteOption = Lens.lens (\DeleteWorkGroup' {recursiveDeleteOption} -> recursiveDeleteOption) (\s@DeleteWorkGroup' {} a -> s {recursiveDeleteOption = a} :: DeleteWorkGroup)

-- | The unique name of the workgroup to delete.
deleteWorkGroup_workGroup :: Lens.Lens' DeleteWorkGroup Prelude.Text
deleteWorkGroup_workGroup = Lens.lens (\DeleteWorkGroup' {workGroup} -> workGroup) (\s@DeleteWorkGroup' {} a -> s {workGroup = a} :: DeleteWorkGroup)

instance Prelude.AWSRequest DeleteWorkGroup where
  type Rs DeleteWorkGroup = DeleteWorkGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkGroup

instance Prelude.NFData DeleteWorkGroup

instance Prelude.ToHeaders DeleteWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonAthena.DeleteWorkGroup" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWorkGroup where
  toJSON DeleteWorkGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RecursiveDeleteOption" Prelude..=)
              Prelude.<$> recursiveDeleteOption,
            Prelude.Just ("WorkGroup" Prelude..= workGroup)
          ]
      )

instance Prelude.ToPath DeleteWorkGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkGroupResponse' smart constructor.
data DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkGroupResponse_httpStatus' - The response's http status code.
newDeleteWorkGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteWorkGroupResponse
newDeleteWorkGroupResponse pHttpStatus_ =
  DeleteWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkGroupResponse_httpStatus :: Lens.Lens' DeleteWorkGroupResponse Prelude.Int
deleteWorkGroupResponse_httpStatus = Lens.lens (\DeleteWorkGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkGroupResponse' {} a -> s {httpStatus = a} :: DeleteWorkGroupResponse)

instance Prelude.NFData DeleteWorkGroupResponse
