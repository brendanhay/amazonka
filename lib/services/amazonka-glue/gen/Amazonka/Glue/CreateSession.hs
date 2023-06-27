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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createSession_connections,
    createSession_defaultArguments,
    createSession_description,
    createSession_glueVersion,
    createSession_idleTimeout,
    createSession_maxCapacity,
    createSession_numberOfWorkers,
    createSession_requestOrigin,
    createSession_securityConfiguration,
    createSession_tags,
    createSession_timeout,
    createSession_workerType,
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
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create a new session.
--
-- /See:/ 'newCreateSession' smart constructor.
data CreateSession = CreateSession'
  { -- | The number of connections to use for the session.
    connections :: Prelude.Maybe ConnectionsList,
    -- | A map array of key-value pairs. Max is 75 pairs.
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the session.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The GlueVersion must be greater than 2.0.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes when idle before session times out. Default for
    -- Spark ETL jobs is value of Timeout. Consult the documentation for other
    -- job types.
    idleTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The number of Glue data processing units (DPUs) that can be allocated
    -- when the job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB memory.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The number of workers of a defined @WorkerType@ to use for the session.
    numberOfWorkers :: Prelude.Maybe Prelude.Int,
    -- | The origin of the request.
    requestOrigin :: Prelude.Maybe Prelude.Text,
    -- | The name of the SecurityConfiguration structure to be used with the
    -- session
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The map of key value pairs (tags) belonging to the session.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of minutes before session times out. Default for Spark ETL
    -- jobs is 48 hours (2880 minutes), the maximum session lifetime for this
    -- job type. Consult the documentation for other job types.
    timeout :: Prelude.Maybe Prelude.Natural,
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
-- 'connections', 'createSession_connections' - The number of connections to use for the session.
--
-- 'defaultArguments', 'createSession_defaultArguments' - A map array of key-value pairs. Max is 75 pairs.
--
-- 'description', 'createSession_description' - The description of the session.
--
-- 'glueVersion', 'createSession_glueVersion' - The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
--
-- 'idleTimeout', 'createSession_idleTimeout' - The number of minutes when idle before session times out. Default for
-- Spark ETL jobs is value of Timeout. Consult the documentation for other
-- job types.
--
-- 'maxCapacity', 'createSession_maxCapacity' - The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
--
-- 'numberOfWorkers', 'createSession_numberOfWorkers' - The number of workers of a defined @WorkerType@ to use for the session.
--
-- 'requestOrigin', 'createSession_requestOrigin' - The origin of the request.
--
-- 'securityConfiguration', 'createSession_securityConfiguration' - The name of the SecurityConfiguration structure to be used with the
-- session
--
-- 'tags', 'createSession_tags' - The map of key value pairs (tags) belonging to the session.
--
-- 'timeout', 'createSession_timeout' - The number of minutes before session times out. Default for Spark ETL
-- jobs is 48 hours (2880 minutes), the maximum session lifetime for this
-- job type. Consult the documentation for other job types.
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
    { connections = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      description = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      idleTimeout = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      numberOfWorkers = Prelude.Nothing,
      requestOrigin = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeout = Prelude.Nothing,
      workerType = Prelude.Nothing,
      id = pId_,
      role' = pRole_,
      command = pCommand_
    }

-- | The number of connections to use for the session.
createSession_connections :: Lens.Lens' CreateSession (Prelude.Maybe ConnectionsList)
createSession_connections = Lens.lens (\CreateSession' {connections} -> connections) (\s@CreateSession' {} a -> s {connections = a} :: CreateSession)

-- | A map array of key-value pairs. Max is 75 pairs.
createSession_defaultArguments :: Lens.Lens' CreateSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSession_defaultArguments = Lens.lens (\CreateSession' {defaultArguments} -> defaultArguments) (\s@CreateSession' {} a -> s {defaultArguments = a} :: CreateSession) Prelude.. Lens.mapping Lens.coerced

-- | The description of the session.
createSession_description :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_description = Lens.lens (\CreateSession' {description} -> description) (\s@CreateSession' {} a -> s {description = a} :: CreateSession)

-- | The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
createSession_glueVersion :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_glueVersion = Lens.lens (\CreateSession' {glueVersion} -> glueVersion) (\s@CreateSession' {} a -> s {glueVersion = a} :: CreateSession)

-- | The number of minutes when idle before session times out. Default for
-- Spark ETL jobs is value of Timeout. Consult the documentation for other
-- job types.
createSession_idleTimeout :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Natural)
createSession_idleTimeout = Lens.lens (\CreateSession' {idleTimeout} -> idleTimeout) (\s@CreateSession' {} a -> s {idleTimeout = a} :: CreateSession)

-- | The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
createSession_maxCapacity :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Double)
createSession_maxCapacity = Lens.lens (\CreateSession' {maxCapacity} -> maxCapacity) (\s@CreateSession' {} a -> s {maxCapacity = a} :: CreateSession)

-- | The number of workers of a defined @WorkerType@ to use for the session.
createSession_numberOfWorkers :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Int)
createSession_numberOfWorkers = Lens.lens (\CreateSession' {numberOfWorkers} -> numberOfWorkers) (\s@CreateSession' {} a -> s {numberOfWorkers = a} :: CreateSession)

-- | The origin of the request.
createSession_requestOrigin :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_requestOrigin = Lens.lens (\CreateSession' {requestOrigin} -> requestOrigin) (\s@CreateSession' {} a -> s {requestOrigin = a} :: CreateSession)

-- | The name of the SecurityConfiguration structure to be used with the
-- session
createSession_securityConfiguration :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Text)
createSession_securityConfiguration = Lens.lens (\CreateSession' {securityConfiguration} -> securityConfiguration) (\s@CreateSession' {} a -> s {securityConfiguration = a} :: CreateSession)

-- | The map of key value pairs (tags) belonging to the session.
createSession_tags :: Lens.Lens' CreateSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSession_tags = Lens.lens (\CreateSession' {tags} -> tags) (\s@CreateSession' {} a -> s {tags = a} :: CreateSession) Prelude.. Lens.mapping Lens.coerced

-- | The number of minutes before session times out. Default for Spark ETL
-- jobs is 48 hours (2880 minutes), the maximum session lifetime for this
-- job type. Consult the documentation for other job types.
createSession_timeout :: Lens.Lens' CreateSession (Prelude.Maybe Prelude.Natural)
createSession_timeout = Lens.lens (\CreateSession' {timeout} -> timeout) (\s@CreateSession' {} a -> s {timeout = a} :: CreateSession)

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
            Prelude.<$> (x Data..?> "Session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSession where
  hashWithSalt _salt CreateSession' {..} =
    _salt
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` idleTimeout
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` numberOfWorkers
      `Prelude.hashWithSalt` requestOrigin
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` workerType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` command

instance Prelude.NFData CreateSession where
  rnf CreateSession' {..} =
    Prelude.rnf connections
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf idleTimeout
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf numberOfWorkers
      `Prelude.seq` Prelude.rnf requestOrigin
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf workerType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf command

instance Data.ToHeaders CreateSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.CreateSession" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSession where
  toJSON CreateSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Connections" Data..=) Prelude.<$> connections,
            ("DefaultArguments" Data..=)
              Prelude.<$> defaultArguments,
            ("Description" Data..=) Prelude.<$> description,
            ("GlueVersion" Data..=) Prelude.<$> glueVersion,
            ("IdleTimeout" Data..=) Prelude.<$> idleTimeout,
            ("MaxCapacity" Data..=) Prelude.<$> maxCapacity,
            ("NumberOfWorkers" Data..=)
              Prelude.<$> numberOfWorkers,
            ("RequestOrigin" Data..=) Prelude.<$> requestOrigin,
            ("SecurityConfiguration" Data..=)
              Prelude.<$> securityConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Timeout" Data..=) Prelude.<$> timeout,
            ("WorkerType" Data..=) Prelude.<$> workerType,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("Command" Data..= command)
          ]
      )

instance Data.ToPath CreateSession where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSession where
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
