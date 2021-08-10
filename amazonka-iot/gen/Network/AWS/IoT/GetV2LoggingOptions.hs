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
-- Module      : Network.AWS.IoT.GetV2LoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the fine grained logging options.
module Network.AWS.IoT.GetV2LoggingOptions
  ( -- * Creating a Request
    GetV2LoggingOptions (..),
    newGetV2LoggingOptions,

    -- * Destructuring the Response
    GetV2LoggingOptionsResponse (..),
    newGetV2LoggingOptionsResponse,

    -- * Response Lenses
    getV2LoggingOptionsResponse_roleArn,
    getV2LoggingOptionsResponse_disableAllLogs,
    getV2LoggingOptionsResponse_defaultLogLevel,
    getV2LoggingOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetV2LoggingOptions' smart constructor.
data GetV2LoggingOptions = GetV2LoggingOptions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetV2LoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetV2LoggingOptions ::
  GetV2LoggingOptions
newGetV2LoggingOptions = GetV2LoggingOptions'

instance Core.AWSRequest GetV2LoggingOptions where
  type
    AWSResponse GetV2LoggingOptions =
      GetV2LoggingOptionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetV2LoggingOptionsResponse'
            Prelude.<$> (x Core..?> "roleArn")
            Prelude.<*> (x Core..?> "disableAllLogs")
            Prelude.<*> (x Core..?> "defaultLogLevel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetV2LoggingOptions

instance Prelude.NFData GetV2LoggingOptions

instance Core.ToHeaders GetV2LoggingOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetV2LoggingOptions where
  toPath = Prelude.const "/v2LoggingOptions"

instance Core.ToQuery GetV2LoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetV2LoggingOptionsResponse' smart constructor.
data GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse'
  { -- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Disables all logs.
    disableAllLogs :: Prelude.Maybe Prelude.Bool,
    -- | The default log level.
    defaultLogLevel :: Prelude.Maybe LogLevel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetV2LoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getV2LoggingOptionsResponse_roleArn' - The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
--
-- 'disableAllLogs', 'getV2LoggingOptionsResponse_disableAllLogs' - Disables all logs.
--
-- 'defaultLogLevel', 'getV2LoggingOptionsResponse_defaultLogLevel' - The default log level.
--
-- 'httpStatus', 'getV2LoggingOptionsResponse_httpStatus' - The response's http status code.
newGetV2LoggingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetV2LoggingOptionsResponse
newGetV2LoggingOptionsResponse pHttpStatus_ =
  GetV2LoggingOptionsResponse'
    { roleArn =
        Prelude.Nothing,
      disableAllLogs = Prelude.Nothing,
      defaultLogLevel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
getV2LoggingOptionsResponse_roleArn :: Lens.Lens' GetV2LoggingOptionsResponse (Prelude.Maybe Prelude.Text)
getV2LoggingOptionsResponse_roleArn = Lens.lens (\GetV2LoggingOptionsResponse' {roleArn} -> roleArn) (\s@GetV2LoggingOptionsResponse' {} a -> s {roleArn = a} :: GetV2LoggingOptionsResponse)

-- | Disables all logs.
getV2LoggingOptionsResponse_disableAllLogs :: Lens.Lens' GetV2LoggingOptionsResponse (Prelude.Maybe Prelude.Bool)
getV2LoggingOptionsResponse_disableAllLogs = Lens.lens (\GetV2LoggingOptionsResponse' {disableAllLogs} -> disableAllLogs) (\s@GetV2LoggingOptionsResponse' {} a -> s {disableAllLogs = a} :: GetV2LoggingOptionsResponse)

-- | The default log level.
getV2LoggingOptionsResponse_defaultLogLevel :: Lens.Lens' GetV2LoggingOptionsResponse (Prelude.Maybe LogLevel)
getV2LoggingOptionsResponse_defaultLogLevel = Lens.lens (\GetV2LoggingOptionsResponse' {defaultLogLevel} -> defaultLogLevel) (\s@GetV2LoggingOptionsResponse' {} a -> s {defaultLogLevel = a} :: GetV2LoggingOptionsResponse)

-- | The response's http status code.
getV2LoggingOptionsResponse_httpStatus :: Lens.Lens' GetV2LoggingOptionsResponse Prelude.Int
getV2LoggingOptionsResponse_httpStatus = Lens.lens (\GetV2LoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@GetV2LoggingOptionsResponse' {} a -> s {httpStatus = a} :: GetV2LoggingOptionsResponse)

instance Prelude.NFData GetV2LoggingOptionsResponse
