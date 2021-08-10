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
-- Module      : Network.AWS.CloudFront.GetRealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a real-time log configuration.
--
-- To get a real-time log configuration, you can provide the
-- configuration’s name or its Amazon Resource Name (ARN). You must provide
-- at least one. If you provide both, CloudFront uses the name to identify
-- the real-time log configuration to get.
module Network.AWS.CloudFront.GetRealtimeLogConfig
  ( -- * Creating a Request
    GetRealtimeLogConfig (..),
    newGetRealtimeLogConfig,

    -- * Request Lenses
    getRealtimeLogConfig_arn,
    getRealtimeLogConfig_name,

    -- * Destructuring the Response
    GetRealtimeLogConfigResponse (..),
    newGetRealtimeLogConfigResponse,

    -- * Response Lenses
    getRealtimeLogConfigResponse_realtimeLogConfig,
    getRealtimeLogConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRealtimeLogConfig' smart constructor.
data GetRealtimeLogConfig = GetRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of the real-time log configuration to
    -- get.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the real-time log configuration to get.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRealtimeLogConfig_arn' - The Amazon Resource Name (ARN) of the real-time log configuration to
-- get.
--
-- 'name', 'getRealtimeLogConfig_name' - The name of the real-time log configuration to get.
newGetRealtimeLogConfig ::
  GetRealtimeLogConfig
newGetRealtimeLogConfig =
  GetRealtimeLogConfig'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the real-time log configuration to
-- get.
getRealtimeLogConfig_arn :: Lens.Lens' GetRealtimeLogConfig (Prelude.Maybe Prelude.Text)
getRealtimeLogConfig_arn = Lens.lens (\GetRealtimeLogConfig' {arn} -> arn) (\s@GetRealtimeLogConfig' {} a -> s {arn = a} :: GetRealtimeLogConfig)

-- | The name of the real-time log configuration to get.
getRealtimeLogConfig_name :: Lens.Lens' GetRealtimeLogConfig (Prelude.Maybe Prelude.Text)
getRealtimeLogConfig_name = Lens.lens (\GetRealtimeLogConfig' {name} -> name) (\s@GetRealtimeLogConfig' {} a -> s {name = a} :: GetRealtimeLogConfig)

instance Core.AWSRequest GetRealtimeLogConfig where
  type
    AWSResponse GetRealtimeLogConfig =
      GetRealtimeLogConfigResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetRealtimeLogConfigResponse'
            Prelude.<$> (x Core..@? "RealtimeLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRealtimeLogConfig

instance Prelude.NFData GetRealtimeLogConfig

instance Core.ToElement GetRealtimeLogConfig where
  toElement =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}GetRealtimeLogConfigRequest"

instance Core.ToHeaders GetRealtimeLogConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetRealtimeLogConfig where
  toPath =
    Prelude.const
      "/2020-05-31/get-realtime-log-config/"

instance Core.ToQuery GetRealtimeLogConfig where
  toQuery = Prelude.const Prelude.mempty

instance Core.ToXML GetRealtimeLogConfig where
  toXML GetRealtimeLogConfig' {..} =
    Prelude.mconcat
      ["ARN" Core.@= arn, "Name" Core.@= name]

-- | /See:/ 'newGetRealtimeLogConfigResponse' smart constructor.
data GetRealtimeLogConfigResponse = GetRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Prelude.Maybe RealtimeLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeLogConfig', 'getRealtimeLogConfigResponse_realtimeLogConfig' - A real-time log configuration.
--
-- 'httpStatus', 'getRealtimeLogConfigResponse_httpStatus' - The response's http status code.
newGetRealtimeLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRealtimeLogConfigResponse
newGetRealtimeLogConfigResponse pHttpStatus_ =
  GetRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A real-time log configuration.
getRealtimeLogConfigResponse_realtimeLogConfig :: Lens.Lens' GetRealtimeLogConfigResponse (Prelude.Maybe RealtimeLogConfig)
getRealtimeLogConfigResponse_realtimeLogConfig = Lens.lens (\GetRealtimeLogConfigResponse' {realtimeLogConfig} -> realtimeLogConfig) (\s@GetRealtimeLogConfigResponse' {} a -> s {realtimeLogConfig = a} :: GetRealtimeLogConfigResponse)

-- | The response's http status code.
getRealtimeLogConfigResponse_httpStatus :: Lens.Lens' GetRealtimeLogConfigResponse Prelude.Int
getRealtimeLogConfigResponse_httpStatus = Lens.lens (\GetRealtimeLogConfigResponse' {httpStatus} -> httpStatus) (\s@GetRealtimeLogConfigResponse' {} a -> s {httpStatus = a} :: GetRealtimeLogConfigResponse)

instance Prelude.NFData GetRealtimeLogConfigResponse
