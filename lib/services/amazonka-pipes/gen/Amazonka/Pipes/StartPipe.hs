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
-- Module      : Amazonka.Pipes.StartPipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Start an existing pipe.
module Amazonka.Pipes.StartPipe
  ( -- * Creating a Request
    StartPipe (..),
    newStartPipe,

    -- * Request Lenses
    startPipe_name,

    -- * Destructuring the Response
    StartPipeResponse (..),
    newStartPipeResponse,

    -- * Response Lenses
    startPipeResponse_arn,
    startPipeResponse_creationTime,
    startPipeResponse_currentState,
    startPipeResponse_desiredState,
    startPipeResponse_lastModifiedTime,
    startPipeResponse_name,
    startPipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartPipe' smart constructor.
data StartPipe = StartPipe'
  { -- | The name of the pipe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startPipe_name' - The name of the pipe.
newStartPipe ::
  -- | 'name'
  Prelude.Text ->
  StartPipe
newStartPipe pName_ = StartPipe' {name = pName_}

-- | The name of the pipe.
startPipe_name :: Lens.Lens' StartPipe Prelude.Text
startPipe_name = Lens.lens (\StartPipe' {name} -> name) (\s@StartPipe' {} a -> s {name = a} :: StartPipe)

instance Core.AWSRequest StartPipe where
  type AWSResponse StartPipe = StartPipeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartPipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartPipe where
  hashWithSalt _salt StartPipe' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData StartPipe where
  rnf StartPipe' {..} = Prelude.rnf name

instance Data.ToHeaders StartPipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartPipe where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartPipe where
  toPath StartPipe' {..} =
    Prelude.mconcat
      ["/v1/pipes/", Data.toBS name, "/start"]

instance Data.ToQuery StartPipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartPipeResponse' smart constructor.
data StartPipeResponse = StartPipeResponse'
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
-- Create a value of 'StartPipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startPipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'startPipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'startPipeResponse_currentState' - The state the pipe is in.
--
-- 'desiredState', 'startPipeResponse_desiredState' - The state the pipe should be in.
--
-- 'lastModifiedTime', 'startPipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'startPipeResponse_name' - The name of the pipe.
--
-- 'httpStatus', 'startPipeResponse_httpStatus' - The response's http status code.
newStartPipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartPipeResponse
newStartPipeResponse pHttpStatus_ =
  StartPipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
startPipeResponse_arn :: Lens.Lens' StartPipeResponse (Prelude.Maybe Prelude.Text)
startPipeResponse_arn = Lens.lens (\StartPipeResponse' {arn} -> arn) (\s@StartPipeResponse' {} a -> s {arn = a} :: StartPipeResponse)

-- | The time the pipe was created.
startPipeResponse_creationTime :: Lens.Lens' StartPipeResponse (Prelude.Maybe Prelude.UTCTime)
startPipeResponse_creationTime = Lens.lens (\StartPipeResponse' {creationTime} -> creationTime) (\s@StartPipeResponse' {} a -> s {creationTime = a} :: StartPipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
startPipeResponse_currentState :: Lens.Lens' StartPipeResponse (Prelude.Maybe PipeState)
startPipeResponse_currentState = Lens.lens (\StartPipeResponse' {currentState} -> currentState) (\s@StartPipeResponse' {} a -> s {currentState = a} :: StartPipeResponse)

-- | The state the pipe should be in.
startPipeResponse_desiredState :: Lens.Lens' StartPipeResponse (Prelude.Maybe RequestedPipeState)
startPipeResponse_desiredState = Lens.lens (\StartPipeResponse' {desiredState} -> desiredState) (\s@StartPipeResponse' {} a -> s {desiredState = a} :: StartPipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
startPipeResponse_lastModifiedTime :: Lens.Lens' StartPipeResponse (Prelude.Maybe Prelude.UTCTime)
startPipeResponse_lastModifiedTime = Lens.lens (\StartPipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@StartPipeResponse' {} a -> s {lastModifiedTime = a} :: StartPipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
startPipeResponse_name :: Lens.Lens' StartPipeResponse (Prelude.Maybe Prelude.Text)
startPipeResponse_name = Lens.lens (\StartPipeResponse' {name} -> name) (\s@StartPipeResponse' {} a -> s {name = a} :: StartPipeResponse)

-- | The response's http status code.
startPipeResponse_httpStatus :: Lens.Lens' StartPipeResponse Prelude.Int
startPipeResponse_httpStatus = Lens.lens (\StartPipeResponse' {httpStatus} -> httpStatus) (\s@StartPipeResponse' {} a -> s {httpStatus = a} :: StartPipeResponse)

instance Prelude.NFData StartPipeResponse where
  rnf StartPipeResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf currentState `Prelude.seq`
          Prelude.rnf desiredState `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf httpStatus
