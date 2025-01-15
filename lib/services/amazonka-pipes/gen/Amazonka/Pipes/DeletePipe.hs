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
-- Module      : Amazonka.Pipes.DeletePipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an existing pipe. For more information about pipes, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-pipes.html Amazon EventBridge Pipes>
-- in the Amazon EventBridge User Guide.
module Amazonka.Pipes.DeletePipe
  ( -- * Creating a Request
    DeletePipe (..),
    newDeletePipe,

    -- * Request Lenses
    deletePipe_name,

    -- * Destructuring the Response
    DeletePipeResponse (..),
    newDeletePipeResponse,

    -- * Response Lenses
    deletePipeResponse_arn,
    deletePipeResponse_creationTime,
    deletePipeResponse_currentState,
    deletePipeResponse_desiredState,
    deletePipeResponse_lastModifiedTime,
    deletePipeResponse_name,
    deletePipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePipe' smart constructor.
data DeletePipe = DeletePipe'
  { -- | The name of the pipe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deletePipe_name' - The name of the pipe.
newDeletePipe ::
  -- | 'name'
  Prelude.Text ->
  DeletePipe
newDeletePipe pName_ = DeletePipe' {name = pName_}

-- | The name of the pipe.
deletePipe_name :: Lens.Lens' DeletePipe Prelude.Text
deletePipe_name = Lens.lens (\DeletePipe' {name} -> name) (\s@DeletePipe' {} a -> s {name = a} :: DeletePipe)

instance Core.AWSRequest DeletePipe where
  type AWSResponse DeletePipe = DeletePipeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePipe where
  hashWithSalt _salt DeletePipe' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeletePipe where
  rnf DeletePipe' {..} = Prelude.rnf name

instance Data.ToHeaders DeletePipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePipe where
  toPath DeletePipe' {..} =
    Prelude.mconcat ["/v1/pipes/", Data.toBS name]

instance Data.ToQuery DeletePipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePipeResponse' smart constructor.
data DeletePipeResponse = DeletePipeResponse'
  { -- | The ARN of the pipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the pipe was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeStateDescribeResponse,
    -- | When the pipe was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deletePipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'deletePipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'deletePipeResponse_currentState' - The state the pipe is in.
--
-- 'desiredState', 'deletePipeResponse_desiredState' - The state the pipe should be in.
--
-- 'lastModifiedTime', 'deletePipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'deletePipeResponse_name' - The name of the pipe.
--
-- 'httpStatus', 'deletePipeResponse_httpStatus' - The response's http status code.
newDeletePipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePipeResponse
newDeletePipeResponse pHttpStatus_ =
  DeletePipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
deletePipeResponse_arn :: Lens.Lens' DeletePipeResponse (Prelude.Maybe Prelude.Text)
deletePipeResponse_arn = Lens.lens (\DeletePipeResponse' {arn} -> arn) (\s@DeletePipeResponse' {} a -> s {arn = a} :: DeletePipeResponse)

-- | The time the pipe was created.
deletePipeResponse_creationTime :: Lens.Lens' DeletePipeResponse (Prelude.Maybe Prelude.UTCTime)
deletePipeResponse_creationTime = Lens.lens (\DeletePipeResponse' {creationTime} -> creationTime) (\s@DeletePipeResponse' {} a -> s {creationTime = a} :: DeletePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
deletePipeResponse_currentState :: Lens.Lens' DeletePipeResponse (Prelude.Maybe PipeState)
deletePipeResponse_currentState = Lens.lens (\DeletePipeResponse' {currentState} -> currentState) (\s@DeletePipeResponse' {} a -> s {currentState = a} :: DeletePipeResponse)

-- | The state the pipe should be in.
deletePipeResponse_desiredState :: Lens.Lens' DeletePipeResponse (Prelude.Maybe RequestedPipeStateDescribeResponse)
deletePipeResponse_desiredState = Lens.lens (\DeletePipeResponse' {desiredState} -> desiredState) (\s@DeletePipeResponse' {} a -> s {desiredState = a} :: DeletePipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
deletePipeResponse_lastModifiedTime :: Lens.Lens' DeletePipeResponse (Prelude.Maybe Prelude.UTCTime)
deletePipeResponse_lastModifiedTime = Lens.lens (\DeletePipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DeletePipeResponse' {} a -> s {lastModifiedTime = a} :: DeletePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
deletePipeResponse_name :: Lens.Lens' DeletePipeResponse (Prelude.Maybe Prelude.Text)
deletePipeResponse_name = Lens.lens (\DeletePipeResponse' {name} -> name) (\s@DeletePipeResponse' {} a -> s {name = a} :: DeletePipeResponse)

-- | The response's http status code.
deletePipeResponse_httpStatus :: Lens.Lens' DeletePipeResponse Prelude.Int
deletePipeResponse_httpStatus = Lens.lens (\DeletePipeResponse' {httpStatus} -> httpStatus) (\s@DeletePipeResponse' {} a -> s {httpStatus = a} :: DeletePipeResponse)

instance Prelude.NFData DeletePipeResponse where
  rnf DeletePipeResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf currentState `Prelude.seq`
          Prelude.rnf desiredState `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf httpStatus
