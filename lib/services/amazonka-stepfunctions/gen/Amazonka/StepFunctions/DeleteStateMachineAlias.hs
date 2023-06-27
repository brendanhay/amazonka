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
-- Module      : Amazonka.StepFunctions.DeleteStateMachineAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>.
--
-- After you delete a state machine alias, you can\'t use it to start
-- executions. When you delete a state machine alias, Step Functions
-- doesn\'t delete the state machine versions that alias references.
--
-- __Related operations:__
--
-- -   CreateStateMachineAlias
--
-- -   DescribeStateMachineAlias
--
-- -   ListStateMachineAliases
--
-- -   UpdateStateMachineAlias
module Amazonka.StepFunctions.DeleteStateMachineAlias
  ( -- * Creating a Request
    DeleteStateMachineAlias (..),
    newDeleteStateMachineAlias,

    -- * Request Lenses
    deleteStateMachineAlias_stateMachineAliasArn,

    -- * Destructuring the Response
    DeleteStateMachineAliasResponse (..),
    newDeleteStateMachineAliasResponse,

    -- * Response Lenses
    deleteStateMachineAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDeleteStateMachineAlias' smart constructor.
data DeleteStateMachineAlias = DeleteStateMachineAlias'
  { -- | The Amazon Resource Name (ARN) of the state machine alias to delete.
    stateMachineAliasArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStateMachineAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineAliasArn', 'deleteStateMachineAlias_stateMachineAliasArn' - The Amazon Resource Name (ARN) of the state machine alias to delete.
newDeleteStateMachineAlias ::
  -- | 'stateMachineAliasArn'
  Prelude.Text ->
  DeleteStateMachineAlias
newDeleteStateMachineAlias pStateMachineAliasArn_ =
  DeleteStateMachineAlias'
    { stateMachineAliasArn =
        pStateMachineAliasArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine alias to delete.
deleteStateMachineAlias_stateMachineAliasArn :: Lens.Lens' DeleteStateMachineAlias Prelude.Text
deleteStateMachineAlias_stateMachineAliasArn = Lens.lens (\DeleteStateMachineAlias' {stateMachineAliasArn} -> stateMachineAliasArn) (\s@DeleteStateMachineAlias' {} a -> s {stateMachineAliasArn = a} :: DeleteStateMachineAlias)

instance Core.AWSRequest DeleteStateMachineAlias where
  type
    AWSResponse DeleteStateMachineAlias =
      DeleteStateMachineAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStateMachineAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStateMachineAlias where
  hashWithSalt _salt DeleteStateMachineAlias' {..} =
    _salt `Prelude.hashWithSalt` stateMachineAliasArn

instance Prelude.NFData DeleteStateMachineAlias where
  rnf DeleteStateMachineAlias' {..} =
    Prelude.rnf stateMachineAliasArn

instance Data.ToHeaders DeleteStateMachineAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DeleteStateMachineAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStateMachineAlias where
  toJSON DeleteStateMachineAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "stateMachineAliasArn"
                  Data..= stateMachineAliasArn
              )
          ]
      )

instance Data.ToPath DeleteStateMachineAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStateMachineAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStateMachineAliasResponse' smart constructor.
data DeleteStateMachineAliasResponse = DeleteStateMachineAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStateMachineAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStateMachineAliasResponse_httpStatus' - The response's http status code.
newDeleteStateMachineAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStateMachineAliasResponse
newDeleteStateMachineAliasResponse pHttpStatus_ =
  DeleteStateMachineAliasResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStateMachineAliasResponse_httpStatus :: Lens.Lens' DeleteStateMachineAliasResponse Prelude.Int
deleteStateMachineAliasResponse_httpStatus = Lens.lens (\DeleteStateMachineAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteStateMachineAliasResponse' {} a -> s {httpStatus = a} :: DeleteStateMachineAliasResponse)

instance
  Prelude.NFData
    DeleteStateMachineAliasResponse
  where
  rnf DeleteStateMachineAliasResponse' {..} =
    Prelude.rnf httpStatus
