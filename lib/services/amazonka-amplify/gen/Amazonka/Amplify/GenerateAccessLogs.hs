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
-- Module      : Amazonka.Amplify.GenerateAccessLogs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the website access logs for a specific time range using a
-- presigned URL.
module Amazonka.Amplify.GenerateAccessLogs
  ( -- * Creating a Request
    GenerateAccessLogs (..),
    newGenerateAccessLogs,

    -- * Request Lenses
    generateAccessLogs_endTime,
    generateAccessLogs_startTime,
    generateAccessLogs_domainName,
    generateAccessLogs_appId,

    -- * Destructuring the Response
    GenerateAccessLogsResponse (..),
    newGenerateAccessLogsResponse,

    -- * Response Lenses
    generateAccessLogsResponse_logUrl,
    generateAccessLogsResponse_httpStatus,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the generate access logs request.
--
-- /See:/ 'newGenerateAccessLogs' smart constructor.
data GenerateAccessLogs = GenerateAccessLogs'
  { -- | The time at which the logs should end. The time range specified is
    -- inclusive of the end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The time at which the logs should start. The time range specified is
    -- inclusive of the start time.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the domain.
    domainName :: Prelude.Text,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateAccessLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'generateAccessLogs_endTime' - The time at which the logs should end. The time range specified is
-- inclusive of the end time.
--
-- 'startTime', 'generateAccessLogs_startTime' - The time at which the logs should start. The time range specified is
-- inclusive of the start time.
--
-- 'domainName', 'generateAccessLogs_domainName' - The name of the domain.
--
-- 'appId', 'generateAccessLogs_appId' - The unique ID for an Amplify app.
newGenerateAccessLogs ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'appId'
  Prelude.Text ->
  GenerateAccessLogs
newGenerateAccessLogs pDomainName_ pAppId_ =
  GenerateAccessLogs'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing,
      domainName = pDomainName_,
      appId = pAppId_
    }

-- | The time at which the logs should end. The time range specified is
-- inclusive of the end time.
generateAccessLogs_endTime :: Lens.Lens' GenerateAccessLogs (Prelude.Maybe Prelude.UTCTime)
generateAccessLogs_endTime = Lens.lens (\GenerateAccessLogs' {endTime} -> endTime) (\s@GenerateAccessLogs' {} a -> s {endTime = a} :: GenerateAccessLogs) Prelude.. Lens.mapping Data._Time

-- | The time at which the logs should start. The time range specified is
-- inclusive of the start time.
generateAccessLogs_startTime :: Lens.Lens' GenerateAccessLogs (Prelude.Maybe Prelude.UTCTime)
generateAccessLogs_startTime = Lens.lens (\GenerateAccessLogs' {startTime} -> startTime) (\s@GenerateAccessLogs' {} a -> s {startTime = a} :: GenerateAccessLogs) Prelude.. Lens.mapping Data._Time

-- | The name of the domain.
generateAccessLogs_domainName :: Lens.Lens' GenerateAccessLogs Prelude.Text
generateAccessLogs_domainName = Lens.lens (\GenerateAccessLogs' {domainName} -> domainName) (\s@GenerateAccessLogs' {} a -> s {domainName = a} :: GenerateAccessLogs)

-- | The unique ID for an Amplify app.
generateAccessLogs_appId :: Lens.Lens' GenerateAccessLogs Prelude.Text
generateAccessLogs_appId = Lens.lens (\GenerateAccessLogs' {appId} -> appId) (\s@GenerateAccessLogs' {} a -> s {appId = a} :: GenerateAccessLogs)

instance Core.AWSRequest GenerateAccessLogs where
  type
    AWSResponse GenerateAccessLogs =
      GenerateAccessLogsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateAccessLogsResponse'
            Prelude.<$> (x Data..?> "logUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateAccessLogs where
  hashWithSalt _salt GenerateAccessLogs' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` appId

instance Prelude.NFData GenerateAccessLogs where
  rnf GenerateAccessLogs' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders GenerateAccessLogs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateAccessLogs where
  toJSON GenerateAccessLogs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endTime" Data..=) Prelude.<$> endTime,
            ("startTime" Data..=) Prelude.<$> startTime,
            Prelude.Just ("domainName" Data..= domainName)
          ]
      )

instance Data.ToPath GenerateAccessLogs where
  toPath GenerateAccessLogs' {..} =
    Prelude.mconcat
      ["/apps/", Data.toBS appId, "/accesslogs"]

instance Data.ToQuery GenerateAccessLogs where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the generate access logs request.
--
-- /See:/ 'newGenerateAccessLogsResponse' smart constructor.
data GenerateAccessLogsResponse = GenerateAccessLogsResponse'
  { -- | The pre-signed URL for the requested access logs.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateAccessLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logUrl', 'generateAccessLogsResponse_logUrl' - The pre-signed URL for the requested access logs.
--
-- 'httpStatus', 'generateAccessLogsResponse_httpStatus' - The response's http status code.
newGenerateAccessLogsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateAccessLogsResponse
newGenerateAccessLogsResponse pHttpStatus_ =
  GenerateAccessLogsResponse'
    { logUrl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pre-signed URL for the requested access logs.
generateAccessLogsResponse_logUrl :: Lens.Lens' GenerateAccessLogsResponse (Prelude.Maybe Prelude.Text)
generateAccessLogsResponse_logUrl = Lens.lens (\GenerateAccessLogsResponse' {logUrl} -> logUrl) (\s@GenerateAccessLogsResponse' {} a -> s {logUrl = a} :: GenerateAccessLogsResponse)

-- | The response's http status code.
generateAccessLogsResponse_httpStatus :: Lens.Lens' GenerateAccessLogsResponse Prelude.Int
generateAccessLogsResponse_httpStatus = Lens.lens (\GenerateAccessLogsResponse' {httpStatus} -> httpStatus) (\s@GenerateAccessLogsResponse' {} a -> s {httpStatus = a} :: GenerateAccessLogsResponse)

instance Prelude.NFData GenerateAccessLogsResponse where
  rnf GenerateAccessLogsResponse' {..} =
    Prelude.rnf logUrl
      `Prelude.seq` Prelude.rnf httpStatus
