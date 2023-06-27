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
-- Module      : Amazonka.StepFunctions.DeleteStateMachineVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-version.html version>.
-- After you delete a version, you can\'t call StartExecution using that
-- version\'s ARN or use the version with a state machine
-- <https://docs.aws.amazon.com/step-functions/latest/dg/concepts-state-machine-alias.html alias>.
--
-- Deleting a state machine version won\'t terminate its in-progress
-- executions.
--
-- You can\'t delete a state machine version currently referenced by one or
-- more aliases. Before you delete a version, you must either delete the
-- aliases or update them to point to another state machine version.
--
-- __Related operations:__
--
-- -   PublishStateMachineVersion
--
-- -   ListStateMachineVersions
module Amazonka.StepFunctions.DeleteStateMachineVersion
  ( -- * Creating a Request
    DeleteStateMachineVersion (..),
    newDeleteStateMachineVersion,

    -- * Request Lenses
    deleteStateMachineVersion_stateMachineVersionArn,

    -- * Destructuring the Response
    DeleteStateMachineVersionResponse (..),
    newDeleteStateMachineVersionResponse,

    -- * Response Lenses
    deleteStateMachineVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StepFunctions.Types

-- | /See:/ 'newDeleteStateMachineVersion' smart constructor.
data DeleteStateMachineVersion = DeleteStateMachineVersion'
  { -- | The Amazon Resource Name (ARN) of the state machine version to delete.
    stateMachineVersionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStateMachineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineVersionArn', 'deleteStateMachineVersion_stateMachineVersionArn' - The Amazon Resource Name (ARN) of the state machine version to delete.
newDeleteStateMachineVersion ::
  -- | 'stateMachineVersionArn'
  Prelude.Text ->
  DeleteStateMachineVersion
newDeleteStateMachineVersion pStateMachineVersionArn_ =
  DeleteStateMachineVersion'
    { stateMachineVersionArn =
        pStateMachineVersionArn_
    }

-- | The Amazon Resource Name (ARN) of the state machine version to delete.
deleteStateMachineVersion_stateMachineVersionArn :: Lens.Lens' DeleteStateMachineVersion Prelude.Text
deleteStateMachineVersion_stateMachineVersionArn = Lens.lens (\DeleteStateMachineVersion' {stateMachineVersionArn} -> stateMachineVersionArn) (\s@DeleteStateMachineVersion' {} a -> s {stateMachineVersionArn = a} :: DeleteStateMachineVersion)

instance Core.AWSRequest DeleteStateMachineVersion where
  type
    AWSResponse DeleteStateMachineVersion =
      DeleteStateMachineVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStateMachineVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStateMachineVersion where
  hashWithSalt _salt DeleteStateMachineVersion' {..} =
    _salt `Prelude.hashWithSalt` stateMachineVersionArn

instance Prelude.NFData DeleteStateMachineVersion where
  rnf DeleteStateMachineVersion' {..} =
    Prelude.rnf stateMachineVersionArn

instance Data.ToHeaders DeleteStateMachineVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSStepFunctions.DeleteStateMachineVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStateMachineVersion where
  toJSON DeleteStateMachineVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "stateMachineVersionArn"
                  Data..= stateMachineVersionArn
              )
          ]
      )

instance Data.ToPath DeleteStateMachineVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStateMachineVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStateMachineVersionResponse' smart constructor.
data DeleteStateMachineVersionResponse = DeleteStateMachineVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStateMachineVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStateMachineVersionResponse_httpStatus' - The response's http status code.
newDeleteStateMachineVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStateMachineVersionResponse
newDeleteStateMachineVersionResponse pHttpStatus_ =
  DeleteStateMachineVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStateMachineVersionResponse_httpStatus :: Lens.Lens' DeleteStateMachineVersionResponse Prelude.Int
deleteStateMachineVersionResponse_httpStatus = Lens.lens (\DeleteStateMachineVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteStateMachineVersionResponse' {} a -> s {httpStatus = a} :: DeleteStateMachineVersionResponse)

instance
  Prelude.NFData
    DeleteStateMachineVersionResponse
  where
  rnf DeleteStateMachineVersionResponse' {..} =
    Prelude.rnf httpStatus
