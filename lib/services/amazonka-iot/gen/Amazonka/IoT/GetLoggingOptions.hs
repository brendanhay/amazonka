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
-- Module      : Amazonka.IoT.GetLoggingOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the logging options.
--
-- NOTE: use of this command is not recommended. Use @GetV2LoggingOptions@
-- instead.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetLoggingOptions>
-- action.
module Amazonka.IoT.GetLoggingOptions
  ( -- * Creating a Request
    GetLoggingOptions (..),
    newGetLoggingOptions,

    -- * Destructuring the Response
    GetLoggingOptionsResponse (..),
    newGetLoggingOptionsResponse,

    -- * Response Lenses
    getLoggingOptionsResponse_roleArn,
    getLoggingOptionsResponse_logLevel,
    getLoggingOptionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetLoggingOptions operation.
--
-- /See:/ 'newGetLoggingOptions' smart constructor.
data GetLoggingOptions = GetLoggingOptions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetLoggingOptions ::
  GetLoggingOptions
newGetLoggingOptions = GetLoggingOptions'

instance Core.AWSRequest GetLoggingOptions where
  type
    AWSResponse GetLoggingOptions =
      GetLoggingOptionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingOptionsResponse'
            Prelude.<$> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "logLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggingOptions where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetLoggingOptions where
  rnf _ = ()

instance Data.ToHeaders GetLoggingOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetLoggingOptions where
  toPath = Prelude.const "/loggingOptions"

instance Data.ToQuery GetLoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetLoggingOptions operation.
--
-- /See:/ 'newGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { -- | The ARN of the IAM role that grants access.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The logging level.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getLoggingOptionsResponse_roleArn' - The ARN of the IAM role that grants access.
--
-- 'logLevel', 'getLoggingOptionsResponse_logLevel' - The logging level.
--
-- 'httpStatus', 'getLoggingOptionsResponse_httpStatus' - The response's http status code.
newGetLoggingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggingOptionsResponse
newGetLoggingOptionsResponse pHttpStatus_ =
  GetLoggingOptionsResponse'
    { roleArn =
        Prelude.Nothing,
      logLevel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the IAM role that grants access.
getLoggingOptionsResponse_roleArn :: Lens.Lens' GetLoggingOptionsResponse (Prelude.Maybe Prelude.Text)
getLoggingOptionsResponse_roleArn = Lens.lens (\GetLoggingOptionsResponse' {roleArn} -> roleArn) (\s@GetLoggingOptionsResponse' {} a -> s {roleArn = a} :: GetLoggingOptionsResponse)

-- | The logging level.
getLoggingOptionsResponse_logLevel :: Lens.Lens' GetLoggingOptionsResponse (Prelude.Maybe LogLevel)
getLoggingOptionsResponse_logLevel = Lens.lens (\GetLoggingOptionsResponse' {logLevel} -> logLevel) (\s@GetLoggingOptionsResponse' {} a -> s {logLevel = a} :: GetLoggingOptionsResponse)

-- | The response's http status code.
getLoggingOptionsResponse_httpStatus :: Lens.Lens' GetLoggingOptionsResponse Prelude.Int
getLoggingOptionsResponse_httpStatus = Lens.lens (\GetLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@GetLoggingOptionsResponse' {} a -> s {httpStatus = a} :: GetLoggingOptionsResponse)

instance Prelude.NFData GetLoggingOptionsResponse where
  rnf GetLoggingOptionsResponse' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf httpStatus
