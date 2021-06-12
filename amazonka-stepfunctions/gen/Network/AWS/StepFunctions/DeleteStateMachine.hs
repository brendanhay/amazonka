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
-- Module      : Network.AWS.StepFunctions.DeleteStateMachine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine. This is an asynchronous operation: It sets the
-- state machine\'s status to @DELETING@ and begins the deletion process.
--
-- For @EXPRESS@state machines, the deletion will happen eventually
-- (usually less than a minute). Running executions may emit logs after
-- @DeleteStateMachine@ API is called.
module Network.AWS.StepFunctions.DeleteStateMachine
  ( -- * Creating a Request
    DeleteStateMachine (..),
    newDeleteStateMachine,

    -- * Request Lenses
    deleteStateMachine_stateMachineArn,

    -- * Destructuring the Response
    DeleteStateMachineResponse (..),
    newDeleteStateMachineResponse,

    -- * Response Lenses
    deleteStateMachineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StepFunctions.Types

-- | /See:/ 'newDeleteStateMachine' smart constructor.
data DeleteStateMachine = DeleteStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to delete.
    stateMachineArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStateMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineArn', 'deleteStateMachine_stateMachineArn' - The Amazon Resource Name (ARN) of the state machine to delete.
newDeleteStateMachine ::
  -- | 'stateMachineArn'
  Core.Text ->
  DeleteStateMachine
newDeleteStateMachine pStateMachineArn_ =
  DeleteStateMachine'
    { stateMachineArn =
        pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine to delete.
deleteStateMachine_stateMachineArn :: Lens.Lens' DeleteStateMachine Core.Text
deleteStateMachine_stateMachineArn = Lens.lens (\DeleteStateMachine' {stateMachineArn} -> stateMachineArn) (\s@DeleteStateMachine' {} a -> s {stateMachineArn = a} :: DeleteStateMachine)

instance Core.AWSRequest DeleteStateMachine where
  type
    AWSResponse DeleteStateMachine =
      DeleteStateMachineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStateMachineResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteStateMachine

instance Core.NFData DeleteStateMachine

instance Core.ToHeaders DeleteStateMachine where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSStepFunctions.DeleteStateMachine" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteStateMachine where
  toJSON DeleteStateMachine' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("stateMachineArn" Core..= stateMachineArn)
          ]
      )

instance Core.ToPath DeleteStateMachine where
  toPath = Core.const "/"

instance Core.ToQuery DeleteStateMachine where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteStateMachineResponse' smart constructor.
data DeleteStateMachineResponse = DeleteStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteStateMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStateMachineResponse_httpStatus' - The response's http status code.
newDeleteStateMachineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteStateMachineResponse
newDeleteStateMachineResponse pHttpStatus_ =
  DeleteStateMachineResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStateMachineResponse_httpStatus :: Lens.Lens' DeleteStateMachineResponse Core.Int
deleteStateMachineResponse_httpStatus = Lens.lens (\DeleteStateMachineResponse' {httpStatus} -> httpStatus) (\s@DeleteStateMachineResponse' {} a -> s {httpStatus = a} :: DeleteStateMachineResponse)

instance Core.NFData DeleteStateMachineResponse
