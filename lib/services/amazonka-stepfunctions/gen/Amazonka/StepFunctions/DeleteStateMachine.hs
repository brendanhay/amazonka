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
-- Module      : Amazonka.StepFunctions.DeleteStateMachine
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine. This is an asynchronous operation: It sets the
-- state machine\'s status to @DELETING@ and begins the deletion process.
--
-- If the given state machine Amazon Resource Name (ARN) is a qualified
-- state machine ARN, it will fail with ValidationException.
--
-- A qualified state machine ARN refers to a /Distributed Map state/
-- defined within a state machine. For example, the qualified state machine
-- ARN
-- @arn:partition:states:region:account-id:stateMachine:stateMachineName\/mapStateLabel@
-- refers to a /Distributed Map state/ with a label @mapStateLabel@ in the
-- state machine named @stateMachineName@.
--
-- For @EXPRESS@ state machines, the deletion will happen eventually
-- (usually less than a minute). Running executions may emit logs after
-- @DeleteStateMachine@ API is called.
module Amazonka.StepFunctions.DeleteStateMachine
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDeleteStateMachine' smart constructor.
data DeleteStateMachine = DeleteStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to delete.
    stateMachineArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteStateMachine
newDeleteStateMachine pStateMachineArn_ =
  DeleteStateMachine'
    { stateMachineArn =
        pStateMachineArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine to delete.
deleteStateMachine_stateMachineArn :: Lens.Lens' DeleteStateMachine Prelude.Text
deleteStateMachine_stateMachineArn = Lens.lens (\DeleteStateMachine' {stateMachineArn} -> stateMachineArn) (\s@DeleteStateMachine' {} a -> s {stateMachineArn = a} :: DeleteStateMachine)

instance Core.AWSRequest DeleteStateMachine where
  type
    AWSResponse DeleteStateMachine =
      DeleteStateMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStateMachineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStateMachine where
  hashWithSalt _salt DeleteStateMachine' {..} =
    _salt `Prelude.hashWithSalt` stateMachineArn

instance Prelude.NFData DeleteStateMachine where
  rnf DeleteStateMachine' {..} =
    Prelude.rnf stateMachineArn

instance Data.ToHeaders DeleteStateMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DeleteStateMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStateMachine where
  toJSON DeleteStateMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("stateMachineArn" Data..= stateMachineArn)
          ]
      )

instance Data.ToPath DeleteStateMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStateMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStateMachineResponse' smart constructor.
data DeleteStateMachineResponse = DeleteStateMachineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteStateMachineResponse
newDeleteStateMachineResponse pHttpStatus_ =
  DeleteStateMachineResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStateMachineResponse_httpStatus :: Lens.Lens' DeleteStateMachineResponse Prelude.Int
deleteStateMachineResponse_httpStatus = Lens.lens (\DeleteStateMachineResponse' {httpStatus} -> httpStatus) (\s@DeleteStateMachineResponse' {} a -> s {httpStatus = a} :: DeleteStateMachineResponse)

instance Prelude.NFData DeleteStateMachineResponse where
  rnf DeleteStateMachineResponse' {..} =
    Prelude.rnf httpStatus
