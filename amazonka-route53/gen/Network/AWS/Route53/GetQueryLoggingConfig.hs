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
-- Module      : Network.AWS.Route53.GetQueryLoggingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified configuration for DNS query logging.
--
-- For more information about DNS query logs, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateQueryLoggingConfig.html CreateQueryLoggingConfig>
-- and
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries>.
module Network.AWS.Route53.GetQueryLoggingConfig
  ( -- * Creating a Request
    GetQueryLoggingConfig (..),
    newGetQueryLoggingConfig,

    -- * Request Lenses
    getQueryLoggingConfig_id,

    -- * Destructuring the Response
    GetQueryLoggingConfigResponse (..),
    newGetQueryLoggingConfigResponse,

    -- * Response Lenses
    getQueryLoggingConfigResponse_httpStatus,
    getQueryLoggingConfigResponse_queryLoggingConfig,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | /See:/ 'newGetQueryLoggingConfig' smart constructor.
data GetQueryLoggingConfig = GetQueryLoggingConfig'
  { -- | The ID of the configuration for DNS query logging that you want to get
    -- information about.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryLoggingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getQueryLoggingConfig_id' - The ID of the configuration for DNS query logging that you want to get
-- information about.
newGetQueryLoggingConfig ::
  -- | 'id'
  Prelude.Text ->
  GetQueryLoggingConfig
newGetQueryLoggingConfig pId_ =
  GetQueryLoggingConfig' {id = pId_}

-- | The ID of the configuration for DNS query logging that you want to get
-- information about.
getQueryLoggingConfig_id :: Lens.Lens' GetQueryLoggingConfig Prelude.Text
getQueryLoggingConfig_id = Lens.lens (\GetQueryLoggingConfig' {id} -> id) (\s@GetQueryLoggingConfig' {} a -> s {id = a} :: GetQueryLoggingConfig)

instance Core.AWSRequest GetQueryLoggingConfig where
  type
    AWSResponse GetQueryLoggingConfig =
      GetQueryLoggingConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetQueryLoggingConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "QueryLoggingConfig")
      )

instance Prelude.Hashable GetQueryLoggingConfig

instance Prelude.NFData GetQueryLoggingConfig

instance Core.ToHeaders GetQueryLoggingConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetQueryLoggingConfig where
  toPath GetQueryLoggingConfig' {..} =
    Prelude.mconcat
      ["/2013-04-01/queryloggingconfig/", Core.toBS id]

instance Core.ToQuery GetQueryLoggingConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryLoggingConfigResponse' smart constructor.
data GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains information about the query logging
    -- configuration that you specified in a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig>
    -- request.
    queryLoggingConfig :: QueryLoggingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryLoggingConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getQueryLoggingConfigResponse_httpStatus' - The response's http status code.
--
-- 'queryLoggingConfig', 'getQueryLoggingConfigResponse_queryLoggingConfig' - A complex type that contains information about the query logging
-- configuration that you specified in a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig>
-- request.
newGetQueryLoggingConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryLoggingConfig'
  QueryLoggingConfig ->
  GetQueryLoggingConfigResponse
newGetQueryLoggingConfigResponse
  pHttpStatus_
  pQueryLoggingConfig_ =
    GetQueryLoggingConfigResponse'
      { httpStatus =
          pHttpStatus_,
        queryLoggingConfig = pQueryLoggingConfig_
      }

-- | The response's http status code.
getQueryLoggingConfigResponse_httpStatus :: Lens.Lens' GetQueryLoggingConfigResponse Prelude.Int
getQueryLoggingConfigResponse_httpStatus = Lens.lens (\GetQueryLoggingConfigResponse' {httpStatus} -> httpStatus) (\s@GetQueryLoggingConfigResponse' {} a -> s {httpStatus = a} :: GetQueryLoggingConfigResponse)

-- | A complex type that contains information about the query logging
-- configuration that you specified in a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetQueryLoggingConfig.html GetQueryLoggingConfig>
-- request.
getQueryLoggingConfigResponse_queryLoggingConfig :: Lens.Lens' GetQueryLoggingConfigResponse QueryLoggingConfig
getQueryLoggingConfigResponse_queryLoggingConfig = Lens.lens (\GetQueryLoggingConfigResponse' {queryLoggingConfig} -> queryLoggingConfig) (\s@GetQueryLoggingConfigResponse' {} a -> s {queryLoggingConfig = a} :: GetQueryLoggingConfigResponse)

instance Prelude.NFData GetQueryLoggingConfigResponse
