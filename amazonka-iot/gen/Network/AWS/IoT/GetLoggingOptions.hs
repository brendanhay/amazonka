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
-- Module      : Network.AWS.IoT.GetLoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the logging options.
--
-- NOTE: use of this command is not recommended. Use @GetV2LoggingOptions@
-- instead.
module Network.AWS.IoT.GetLoggingOptions
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetLoggingOptions operation.
--
-- /See:/ 'newGetLoggingOptions' smart constructor.
data GetLoggingOptions = GetLoggingOptions'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingOptionsResponse'
            Core.<$> (x Core..?> "roleArn")
            Core.<*> (x Core..?> "logLevel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLoggingOptions

instance Core.NFData GetLoggingOptions

instance Core.ToHeaders GetLoggingOptions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetLoggingOptions where
  toPath = Core.const "/loggingOptions"

instance Core.ToQuery GetLoggingOptions where
  toQuery = Core.const Core.mempty

-- | The output from the GetLoggingOptions operation.
--
-- /See:/ 'newGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { -- | The ARN of the IAM role that grants access.
    roleArn :: Core.Maybe Core.Text,
    -- | The logging level.
    logLevel :: Core.Maybe LogLevel,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetLoggingOptionsResponse
newGetLoggingOptionsResponse pHttpStatus_ =
  GetLoggingOptionsResponse'
    { roleArn = Core.Nothing,
      logLevel = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the IAM role that grants access.
getLoggingOptionsResponse_roleArn :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe Core.Text)
getLoggingOptionsResponse_roleArn = Lens.lens (\GetLoggingOptionsResponse' {roleArn} -> roleArn) (\s@GetLoggingOptionsResponse' {} a -> s {roleArn = a} :: GetLoggingOptionsResponse)

-- | The logging level.
getLoggingOptionsResponse_logLevel :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe LogLevel)
getLoggingOptionsResponse_logLevel = Lens.lens (\GetLoggingOptionsResponse' {logLevel} -> logLevel) (\s@GetLoggingOptionsResponse' {} a -> s {logLevel = a} :: GetLoggingOptionsResponse)

-- | The response's http status code.
getLoggingOptionsResponse_httpStatus :: Lens.Lens' GetLoggingOptionsResponse Core.Int
getLoggingOptionsResponse_httpStatus = Lens.lens (\GetLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@GetLoggingOptionsResponse' {} a -> s {httpStatus = a} :: GetLoggingOptionsResponse)

instance Core.NFData GetLoggingOptionsResponse
