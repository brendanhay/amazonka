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
-- Module      : Amazonka.Pipes.StopPipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stop an existing pipe.
module Amazonka.Pipes.StopPipe
  ( -- * Creating a Request
    StopPipe (..),
    newStopPipe,

    -- * Request Lenses
    stopPipe_name,

    -- * Destructuring the Response
    StopPipeResponse (..),
    newStopPipeResponse,

    -- * Response Lenses
    stopPipeResponse_arn,
    stopPipeResponse_creationTime,
    stopPipeResponse_currentState,
    stopPipeResponse_desiredState,
    stopPipeResponse_lastModifiedTime,
    stopPipeResponse_name,
    stopPipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopPipe' smart constructor.
data StopPipe = StopPipe'
  { -- | The name of the pipe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopPipe_name' - The name of the pipe.
newStopPipe ::
  -- | 'name'
  Prelude.Text ->
  StopPipe
newStopPipe pName_ = StopPipe' {name = pName_}

-- | The name of the pipe.
stopPipe_name :: Lens.Lens' StopPipe Prelude.Text
stopPipe_name = Lens.lens (\StopPipe' {name} -> name) (\s@StopPipe' {} a -> s {name = a} :: StopPipe)

instance Core.AWSRequest StopPipe where
  type AWSResponse StopPipe = StopPipeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopPipe where
  hashWithSalt _salt StopPipe' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StopPipe where
  rnf StopPipe' {..} = Prelude.rnf name

instance Data.ToHeaders StopPipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopPipe where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopPipe where
  toPath StopPipe' {..} =
    Prelude.mconcat
      ["/v1/pipes/", Data.toBS name, "/stop"]

instance Data.ToQuery StopPipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopPipeResponse' smart constructor.
data StopPipeResponse = StopPipeResponse'
  { -- | The ARN of the pipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the pipe was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
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
-- Create a value of 'StopPipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'stopPipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'stopPipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'stopPipeResponse_currentState' - The state the pipe is in.
--
-- 'desiredState', 'stopPipeResponse_desiredState' - The state the pipe should be in.
--
-- 'lastModifiedTime', 'stopPipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'stopPipeResponse_name' - The name of the pipe.
--
-- 'httpStatus', 'stopPipeResponse_httpStatus' - The response's http status code.
newStopPipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopPipeResponse
newStopPipeResponse pHttpStatus_ =
  StopPipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
stopPipeResponse_arn :: Lens.Lens' StopPipeResponse (Prelude.Maybe Prelude.Text)
stopPipeResponse_arn = Lens.lens (\StopPipeResponse' {arn} -> arn) (\s@StopPipeResponse' {} a -> s {arn = a} :: StopPipeResponse)

-- | The time the pipe was created.
stopPipeResponse_creationTime :: Lens.Lens' StopPipeResponse (Prelude.Maybe Prelude.UTCTime)
stopPipeResponse_creationTime = Lens.lens (\StopPipeResponse' {creationTime} -> creationTime) (\s@StopPipeResponse' {} a -> s {creationTime = a} :: StopPipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
stopPipeResponse_currentState :: Lens.Lens' StopPipeResponse (Prelude.Maybe PipeState)
stopPipeResponse_currentState = Lens.lens (\StopPipeResponse' {currentState} -> currentState) (\s@StopPipeResponse' {} a -> s {currentState = a} :: StopPipeResponse)

-- | The state the pipe should be in.
stopPipeResponse_desiredState :: Lens.Lens' StopPipeResponse (Prelude.Maybe RequestedPipeState)
stopPipeResponse_desiredState = Lens.lens (\StopPipeResponse' {desiredState} -> desiredState) (\s@StopPipeResponse' {} a -> s {desiredState = a} :: StopPipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
stopPipeResponse_lastModifiedTime :: Lens.Lens' StopPipeResponse (Prelude.Maybe Prelude.UTCTime)
stopPipeResponse_lastModifiedTime = Lens.lens (\StopPipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@StopPipeResponse' {} a -> s {lastModifiedTime = a} :: StopPipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
stopPipeResponse_name :: Lens.Lens' StopPipeResponse (Prelude.Maybe Prelude.Text)
stopPipeResponse_name = Lens.lens (\StopPipeResponse' {name} -> name) (\s@StopPipeResponse' {} a -> s {name = a} :: StopPipeResponse)

-- | The response's http status code.
stopPipeResponse_httpStatus :: Lens.Lens' StopPipeResponse Prelude.Int
stopPipeResponse_httpStatus = Lens.lens (\StopPipeResponse' {httpStatus} -> httpStatus) (\s@StopPipeResponse' {} a -> s {httpStatus = a} :: StopPipeResponse)

instance Prelude.NFData StopPipeResponse where
  rnf StopPipeResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
