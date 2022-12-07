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
-- Module      : Amazonka.CloudFront.GetRealtimeLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudFront.GetRealtimeLogConfig
  ( -- * Creating a Request
    GetRealtimeLogConfig (..),
    newGetRealtimeLogConfig,

    -- * Request Lenses
    getRealtimeLogConfig_name,
    getRealtimeLogConfig_arn,

    -- * Destructuring the Response
    GetRealtimeLogConfigResponse (..),
    newGetRealtimeLogConfigResponse,

    -- * Response Lenses
    getRealtimeLogConfigResponse_realtimeLogConfig,
    getRealtimeLogConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRealtimeLogConfig' smart constructor.
data GetRealtimeLogConfig = GetRealtimeLogConfig'
  { -- | The name of the real-time log configuration to get.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration to
    -- get.
    arn :: Prelude.Maybe Prelude.Text
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
-- 'name', 'getRealtimeLogConfig_name' - The name of the real-time log configuration to get.
--
-- 'arn', 'getRealtimeLogConfig_arn' - The Amazon Resource Name (ARN) of the real-time log configuration to
-- get.
newGetRealtimeLogConfig ::
  GetRealtimeLogConfig
newGetRealtimeLogConfig =
  GetRealtimeLogConfig'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing
    }

-- | The name of the real-time log configuration to get.
getRealtimeLogConfig_name :: Lens.Lens' GetRealtimeLogConfig (Prelude.Maybe Prelude.Text)
getRealtimeLogConfig_name = Lens.lens (\GetRealtimeLogConfig' {name} -> name) (\s@GetRealtimeLogConfig' {} a -> s {name = a} :: GetRealtimeLogConfig)

-- | The Amazon Resource Name (ARN) of the real-time log configuration to
-- get.
getRealtimeLogConfig_arn :: Lens.Lens' GetRealtimeLogConfig (Prelude.Maybe Prelude.Text)
getRealtimeLogConfig_arn = Lens.lens (\GetRealtimeLogConfig' {arn} -> arn) (\s@GetRealtimeLogConfig' {} a -> s {arn = a} :: GetRealtimeLogConfig)

instance Core.AWSRequest GetRealtimeLogConfig where
  type
    AWSResponse GetRealtimeLogConfig =
      GetRealtimeLogConfigResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetRealtimeLogConfigResponse'
            Prelude.<$> (x Data..@? "RealtimeLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRealtimeLogConfig where
  hashWithSalt _salt GetRealtimeLogConfig' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData GetRealtimeLogConfig where
  rnf GetRealtimeLogConfig' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf arn

instance Data.ToElement GetRealtimeLogConfig where
  toElement =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}GetRealtimeLogConfigRequest"

instance Data.ToHeaders GetRealtimeLogConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetRealtimeLogConfig where
  toPath =
    Prelude.const
      "/2020-05-31/get-realtime-log-config/"

instance Data.ToQuery GetRealtimeLogConfig where
  toQuery = Prelude.const Prelude.mempty

instance Data.ToXML GetRealtimeLogConfig where
  toXML GetRealtimeLogConfig' {..} =
    Prelude.mconcat
      ["Name" Data.@= name, "ARN" Data.@= arn]

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

instance Prelude.NFData GetRealtimeLogConfigResponse where
  rnf GetRealtimeLogConfigResponse' {..} =
    Prelude.rnf realtimeLogConfig
      `Prelude.seq` Prelude.rnf httpStatus
