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
-- Module      : Network.AWS.SageMaker.DeleteWorkteam
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing work team. This operation can\'t be undone.
module Network.AWS.SageMaker.DeleteWorkteam
  ( -- * Creating a Request
    DeleteWorkteam (..),
    newDeleteWorkteam,

    -- * Request Lenses
    deleteWorkteam_workteamName,

    -- * Destructuring the Response
    DeleteWorkteamResponse (..),
    newDeleteWorkteamResponse,

    -- * Response Lenses
    deleteWorkteamResponse_httpStatus,
    deleteWorkteamResponse_success,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteWorkteam' smart constructor.
data DeleteWorkteam = DeleteWorkteam'
  { -- | The name of the work team to delete.
    workteamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkteam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workteamName', 'deleteWorkteam_workteamName' - The name of the work team to delete.
newDeleteWorkteam ::
  -- | 'workteamName'
  Prelude.Text ->
  DeleteWorkteam
newDeleteWorkteam pWorkteamName_ =
  DeleteWorkteam' {workteamName = pWorkteamName_}

-- | The name of the work team to delete.
deleteWorkteam_workteamName :: Lens.Lens' DeleteWorkteam Prelude.Text
deleteWorkteam_workteamName = Lens.lens (\DeleteWorkteam' {workteamName} -> workteamName) (\s@DeleteWorkteam' {} a -> s {workteamName = a} :: DeleteWorkteam)

instance Prelude.AWSRequest DeleteWorkteam where
  type Rs DeleteWorkteam = DeleteWorkteamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkteamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Success")
      )

instance Prelude.Hashable DeleteWorkteam

instance Prelude.NFData DeleteWorkteam

instance Prelude.ToHeaders DeleteWorkteam where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteWorkteam" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWorkteam where
  toJSON DeleteWorkteam' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkteamName" Prelude..= workteamName)
          ]
      )

instance Prelude.ToPath DeleteWorkteam where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWorkteam where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkteamResponse' smart constructor.
data DeleteWorkteamResponse = DeleteWorkteamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns @true@ if the work team was successfully deleted; otherwise,
    -- returns @false@.
    success :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteWorkteamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkteamResponse_httpStatus' - The response's http status code.
--
-- 'success', 'deleteWorkteamResponse_success' - Returns @true@ if the work team was successfully deleted; otherwise,
-- returns @false@.
newDeleteWorkteamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'success'
  Prelude.Bool ->
  DeleteWorkteamResponse
newDeleteWorkteamResponse pHttpStatus_ pSuccess_ =
  DeleteWorkteamResponse'
    { httpStatus = pHttpStatus_,
      success = pSuccess_
    }

-- | The response's http status code.
deleteWorkteamResponse_httpStatus :: Lens.Lens' DeleteWorkteamResponse Prelude.Int
deleteWorkteamResponse_httpStatus = Lens.lens (\DeleteWorkteamResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkteamResponse' {} a -> s {httpStatus = a} :: DeleteWorkteamResponse)

-- | Returns @true@ if the work team was successfully deleted; otherwise,
-- returns @false@.
deleteWorkteamResponse_success :: Lens.Lens' DeleteWorkteamResponse Prelude.Bool
deleteWorkteamResponse_success = Lens.lens (\DeleteWorkteamResponse' {success} -> success) (\s@DeleteWorkteamResponse' {} a -> s {success = a} :: DeleteWorkteamResponse)

instance Prelude.NFData DeleteWorkteamResponse
