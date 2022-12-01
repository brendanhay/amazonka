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
-- Module      : Amazonka.Glue.CreateSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new session.
module Amazonka.Glue.CreateSession
  ( -- * Creating a Request
    CreateSession (..),
    newCreateSession,

    -- * Request Lenses
    createSession_securityConfiguration,
    createSession_tags,
    createSession_timeout,
    createSession_numberOfWorkers,
    createSession_glueVersion,
    createSession_requestOrigin,
    createSession_idleTimeout,
    createSession_workerType,
    createSession_description,
    createSession_defaultArguments,
    createSession_connections,
    createSession_maxCapacity,
    createSession_id,
    createSession_role,
    createSession_command,

    -- * Destructuring the Response
    CreateSessionResponse (..),
    newCreateSessionResponse,

    -- * Response Lenses
    createSessionResponse_session,
    createSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create a new session.
--
-- /See:/ 'newCreateSession' smart constructor.
data CreateSession = CreateSession'
  { -- | The name of the SecurityConfiguration structure to be used with the
    -- session
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The map of key value pairs (tags) belonging to the session.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of seconds before request times out.
    timeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of workers of a defined @WorkerType@ to use for the session.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The GlueVersion must be greater than 2.0.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds when idle before request times out.
    idleTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The type of predefined worker that is allocated to use for the session.
    -- Accepts a value of Standard, G.1X, G.2X, or G.025X.
    --
    -- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
    --     of memory and a 50GB disk, and 2 executors per worker.
    --
    -- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
    --     of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
    --     of memory, 128 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for memory-intensive jobs.
    --
    -- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
    --     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
    --     recommend this worker type for low volume streaming jobs. This
    --     worker type is only available for Glue version 3.0 streaming jobs.
    workerType :: Prelude.Maybe WorkerType,
    -- | The description of the session.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map array of key-value pairs. Max is 75 pairs.
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of connections to use for the session.
    connections :: Prelude.Maybe ConnectionsList,
    -- | The number of Glue data processing units (DPUs) that can be allocated
    -- when the job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB memory.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The ID of the session request.
    id :: Prelude.Text,
    -- | The IAM Role ARN
    role' :: Prelude.Text,
    -- | The @SessionCommand@ that runs the job.
    command :: SessionCommand
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfiguration', 'createSession_securityConfiguration' - The name of the SecurityConfiguration structure to be used with the
-- session
--
-- 'tags', 'createSession_tags' - The map of key value pairs (tags) belonging to the session.
--
-- 'timeout', 'createSession_timeout' - The number of seconds before request times out.
--
-- 'numberOfWorkers', 'createSession_numberOfWorkers' - The number of workers of a defined @WorkerType@ to use for the session.
--
-- 'glueVersion', 'createSession_glueVersion' - The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
--
-- 'requestOrigin', 'createSession_requestOrigin' - The origin of the request.
--
-- 'idleTimeout', 'createSession_idleTimeout' - The number of seconds when idle before request times out.
--
-- 'workerType', 'createSession_workerType' - The type of predefined worker that is allocated to use for the session.
-- Accepts a value of Standard, G.1X, G.2X, or G.025X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
--
-- 'description', 'createSession_description' - The description of the session.
--
-- 'defaultArguments', 'createSession_defaultArguments' - A map array of key-value pairs. Max is 75 pairs.
--
-- 'connections', 'createSession_connections' - The number of connections to use for the session.
--
-- 'maxCapacity', 'createSession_maxCapacity' - The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
--
-- 'id', 'createSession_id' - The ID of the session request.
--
-- 'role'', 'createSession_role' - The IAM Role ARN
--
-- 'command', 'createSession_command' - The @SessionCommand@ that runs the job.
newCreateSession ::
  -- | 'id'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'command'
  SessionCommand ->
  CreateSession
newCreateSession pId_ pRole_ pCommand_ =
  CreateSession'
    { securityConfiguration =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      requestOrigin = Prelude.Nothing,
      idleTimeout = Prelude.Nothing,
      workerType = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      connections = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      id = pId_,
      role' = pRole_,
      command = pCommand_
    }

-- | The name of the SecurityConfiguration structure to be used with the
-- session
createSession_securityConfiguration :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_securityConfiguration = Lens.lens (\CreateSession' {securityConfiguration} -> securityConfiguration) (\s@CreateSession' {} a -> s {securityConfiguration = a} :: CreateSession)

-- | The map of key value pairs (tags) belonging to the session.
createSession_tags :: Lens.Lens' CreateSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSession_tags = Lens.lens (\CreateSession' {tags} -> tags) (\s@CreateSession' {} a -> s {tags = a} :: CreateSession) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds before request times out.
createSession_timeout :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Natural)
createSession_timeout = Lens.lens (\CreateSession' {timeout} -> timeout) (\s@CreateSession' {} a -> s {timeout = a} :: CreateSession)

-- | The number of workers of a defined @WorkerType@ to use for the session.
createSession_numberOfWorkers :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Int)
createSession_numberOfWorkers = Lens.lens (\CreateSession' {numberOfWorkers} -> numberOfWorkers) (\s@CreateSession' {} a -> s {numberOfWorkers = a} :: CreateSession)

-- | The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
createSession_glueVersion :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_glueVersion = Lens.lens (\CreateSession' {glueVersion} -> glueVersion) (\s@CreateSession' {} a -> s {glueVersion = a} :: CreateSession)

-- | The origin of the request.
createSession_requestOrigin :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_requestOrigin = Lens.lens (\CreateSession' {requestOrigin} -> requestOrigin) (\s@CreateSession' {} a -> s {requestOrigin = a} :: CreateSession)

-- | The number of seconds when idle before request times out.
createSession_idleTimeout :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Natural)
createSession_idleTimeout = Lens.lens (\CreateSession' {idleTimeout} -> idleTimeout) (\s@CreateSession' {} a -> s {idleTimeout = a} :: CreateSession)

-- | The type of predefined worker that is allocated to use for the session.
-- Accepts a value of Standard, G.1X, G.2X, or G.025X.
--
-- -   For the @Standard@ worker type, each worker provides 4 vCPU, 16 GB
--     of memory and a 50GB disk, and 2 executors per worker.
--
-- -   For the @G.1X@ worker type, each worker maps to 1 DPU (4 vCPU, 16 GB
--     of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.2X@ worker type, each worker maps to 2 DPU (8 vCPU, 32 GB
--     of memory, 128 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for memory-intensive jobs.
--
-- -   For the @G.025X@ worker type, each worker maps to 0.25 DPU (2 vCPU,
--     4 GB of memory, 64 GB disk), and provides 1 executor per worker. We
--     recommend this worker type for low volume streaming jobs. This
--     worker type is only available for Glue version 3.0 streaming jobs.
createSession_workerType :: Lens.Lens' CreateSession (Prelude.Maybe WorkerType)
createSession_workerType = Lens.lens (\CreateSession' {workerType} -> workerType) (\s@CreateSession' {} a -> s {workerType = a} :: CreateSession)

-- | The description of the session.
createSession_description :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_description = Lens.lens (\CreateSession' {description} -> description) (\s@CreateSession' {} a -> s {description = a} :: CreateSession)

-- | A map array of key-value pairs. Max is 75 pairs.
createSession_defaultArguments :: Lens.Lens' CreateSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSession_defaultArguments = Lens.lens (\CreateSession' {defaultArguments} -> defaultArguments) (\s@CreateSession' {} a -> s {defaultArguments = a} :: CreateSession) Prelude.. Lens.mapping Lens.coerced

-- | The number of connections to use for the session.
createSession_connections :: Lens.Lens' CreateSession (Prelude.Maybe ConnectionsList)
createSession_connections = Lens.lens (\CreateSession' {connections} -> connections) (\s@CreateSession' {} a -> s {connections = a} :: CreateSession)

-- | The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
createSession_maxCapacity :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Double)
createSession_maxCapacity = Lens.lens (\CreateSession' {maxCapacity} -> maxCapacity) (\s@CreateSession' {} a -> s {maxCapacity = a} :: CreateSession)

-- | The ID of the session request.
createSession_id :: Lens.Lens' CreateSession Prelude.Text
createSession_id = Lens.lens (\CreateSession' {id} -> id) (\s@CreateSession' {} a -> s {id = a} :: CreateSession)

-- | The IAM Role ARN
createSession_role :: Lens.Lens' CreateSession Prelude.Text
createSession_role = Lens.lens (\CreateSession' {role'} -> role') (\s@CreateSession' {} a -> s {role' = a} :: CreateSession)

-- | The @SessionCommand@ that runs the job.
createSession_command :: Lens.Lens' CreateSession SessionCommand
createSession_command = Lens.lens (\CreateSession' {command} -> command) (\s@CreateSession' {} a -> s {command = a} :: CreateSession)

instance Core.AWSRequest CreateSession where
  type
    AWSResponse CreateSession =
      CreateSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSessionResponse'
            Prelude.<$> (x Core..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSession where
  hashWithSalt _salt CreateSession' {..} =
    _salt `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` idleTimeout
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` command

instance Prelude.NFData CreateSession where
  rnf CreateSession' {..} =
    Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf idleTimeout
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf command

instance Core.ToHeaders CreateSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.CreateSession" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSession where
  toJSON CreateSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SecurityConfiguration" Core..=)
              Prelude.<$> securityConfiguration,
            ("Tags" Core..=) Prelude.<$> tags,
            ("Timeout" Core..=) Prelude.<$> timeout,
            ("NumberOfWorkers" Core..=)
              Prelude.<$> numberOfWorkers,
            ("GlueVersion" Core..=) Prelude.<$> glueVersion,
            ("RequestOrigin" Core..=) Prelude.<$> requestOrigin,
            ("IdleTimeout" Core..=) Prelude.<$> idleTimeout,
            ("WorkerType" Core..=) Prelude.<$> workerType,
            ("Description" Core..=) Prelude.<$> description,
            ("DefaultArguments" Core..=)
              Prelude.<$> defaultArguments,
            ("Connections" Core..=) Prelude.<$> connections,
            ("MaxCapacity" Core..=) Prelude.<$> maxCapacity,
            Prelude.Just ("Id" Core..= id),
            Prelude.Just ("Role" Core..= role'),
            Prelude.Just ("Command" Core..= command)
          ]
      )

instance Core.ToPath CreateSession where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSessionResponse' smart constructor.
data CreateSessionResponse = CreateSessionResponse'
  { -- | Returns the session object in the response.
    session :: Prelude.Maybe Session,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'createSessionResponse_session' - Returns the session object in the response.
--
-- 'httpStatus', 'createSessionResponse_httpStatus' - The response's http status code.
newCreateSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSessionResponse
newCreateSessionResponse pHttpStatus_ =
  CreateSessionResponse'
    { session = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the session object in the response.
createSessionResponse_session :: Lens.Lens' CreateSessionResponse (Prelude.Maybe Session)
createSessionResponse_session = Lens.lens (\CreateSessionResponse' {session} -> session) (\s@CreateSessionResponse' {} a -> s {session = a} :: CreateSessionResponse)

-- | The response's http status code.
createSessionResponse_httpStatus :: Lens.Lens' CreateSessionResponse Prelude.Int
createSessionResponse_httpStatus = Lens.lens (\CreateSessionResponse' {httpStatus} -> httpStatus) (\s@CreateSessionResponse' {} a -> s {httpStatus = a} :: CreateSessionResponse)

instance Prelude.NFData CreateSessionResponse where
  rnf CreateSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
