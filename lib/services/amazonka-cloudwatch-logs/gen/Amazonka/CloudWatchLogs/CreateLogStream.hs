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
-- Module      : Amazonka.CloudWatchLogs.CreateLogStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log stream for the specified log group. A log stream is a
-- sequence of log events that originate from a single source, such as an
-- application instance or a resource that is being monitored.
--
-- There is no limit on the number of log streams that you can create for a
-- log group. There is a limit of 50 TPS on @CreateLogStream@ operations,
-- after which transactions are throttled.
--
-- You must use the following guidelines when naming a log stream:
--
-- -   Log stream names must be unique within the log group.
--
-- -   Log stream names can be between 1 and 512 characters long.
--
-- -   Don\'t use \':\' (colon) or \'*\' (asterisk) characters.
module Amazonka.CloudWatchLogs.CreateLogStream
  ( -- * Creating a Request
    CreateLogStream (..),
    newCreateLogStream,

    -- * Request Lenses
    createLogStream_logGroupName,
    createLogStream_logStreamName,

    -- * Destructuring the Response
    CreateLogStreamResponse (..),
    newCreateLogStreamResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLogStream' smart constructor.
data CreateLogStream = CreateLogStream'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The name of the log stream.
    logStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'createLogStream_logGroupName' - The name of the log group.
--
-- 'logStreamName', 'createLogStream_logStreamName' - The name of the log stream.
newCreateLogStream ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'logStreamName'
  Prelude.Text ->
  CreateLogStream
newCreateLogStream pLogGroupName_ pLogStreamName_ =
  CreateLogStream'
    { logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The name of the log group.
createLogStream_logGroupName :: Lens.Lens' CreateLogStream Prelude.Text
createLogStream_logGroupName = Lens.lens (\CreateLogStream' {logGroupName} -> logGroupName) (\s@CreateLogStream' {} a -> s {logGroupName = a} :: CreateLogStream)

-- | The name of the log stream.
createLogStream_logStreamName :: Lens.Lens' CreateLogStream Prelude.Text
createLogStream_logStreamName = Lens.lens (\CreateLogStream' {logStreamName} -> logStreamName) (\s@CreateLogStream' {} a -> s {logStreamName = a} :: CreateLogStream)

instance Core.AWSRequest CreateLogStream where
  type
    AWSResponse CreateLogStream =
      CreateLogStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull CreateLogStreamResponse'

instance Prelude.Hashable CreateLogStream where
  hashWithSalt _salt CreateLogStream' {..} =
    _salt `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` logStreamName

instance Prelude.NFData CreateLogStream where
  rnf CreateLogStream' {..} =
    Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf logStreamName

instance Data.ToHeaders CreateLogStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.CreateLogStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLogStream where
  toJSON CreateLogStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logGroupName" Data..= logGroupName),
            Prelude.Just
              ("logStreamName" Data..= logStreamName)
          ]
      )

instance Data.ToPath CreateLogStream where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLogStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLogStreamResponse' smart constructor.
data CreateLogStreamResponse = CreateLogStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLogStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateLogStreamResponse ::
  CreateLogStreamResponse
newCreateLogStreamResponse = CreateLogStreamResponse'

instance Prelude.NFData CreateLogStreamResponse where
  rnf _ = ()
