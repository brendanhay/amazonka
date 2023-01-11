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
-- Module      : Amazonka.DevOpsGuru.DescribeEventSourcesConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the integration status of services that are integrated with
-- DevOps Guru as Consumer via EventBridge. The one service that can be
-- integrated with DevOps Guru is Amazon CodeGuru Profiler, which can
-- produce proactive recommendations which can be stored and viewed in
-- DevOps Guru.
module Amazonka.DevOpsGuru.DescribeEventSourcesConfig
  ( -- * Creating a Request
    DescribeEventSourcesConfig (..),
    newDescribeEventSourcesConfig,

    -- * Destructuring the Response
    DescribeEventSourcesConfigResponse (..),
    newDescribeEventSourcesConfigResponse,

    -- * Response Lenses
    describeEventSourcesConfigResponse_eventSources,
    describeEventSourcesConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventSourcesConfig' smart constructor.
data DescribeEventSourcesConfig = DescribeEventSourcesConfig'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSourcesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeEventSourcesConfig ::
  DescribeEventSourcesConfig
newDescribeEventSourcesConfig =
  DescribeEventSourcesConfig'

instance Core.AWSRequest DescribeEventSourcesConfig where
  type
    AWSResponse DescribeEventSourcesConfig =
      DescribeEventSourcesConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventSourcesConfigResponse'
            Prelude.<$> (x Data..?> "EventSources")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventSourcesConfig where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeEventSourcesConfig where
  rnf _ = ()

instance Data.ToHeaders DescribeEventSourcesConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventSourcesConfig where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeEventSourcesConfig where
  toPath = Prelude.const "/event-sources"

instance Data.ToQuery DescribeEventSourcesConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventSourcesConfigResponse' smart constructor.
data DescribeEventSourcesConfigResponse = DescribeEventSourcesConfigResponse'
  { -- | Lists the event sources in the configuration.
    eventSources :: Prelude.Maybe EventSourcesConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventSourcesConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSources', 'describeEventSourcesConfigResponse_eventSources' - Lists the event sources in the configuration.
--
-- 'httpStatus', 'describeEventSourcesConfigResponse_httpStatus' - The response's http status code.
newDescribeEventSourcesConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventSourcesConfigResponse
newDescribeEventSourcesConfigResponse pHttpStatus_ =
  DescribeEventSourcesConfigResponse'
    { eventSources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the event sources in the configuration.
describeEventSourcesConfigResponse_eventSources :: Lens.Lens' DescribeEventSourcesConfigResponse (Prelude.Maybe EventSourcesConfig)
describeEventSourcesConfigResponse_eventSources = Lens.lens (\DescribeEventSourcesConfigResponse' {eventSources} -> eventSources) (\s@DescribeEventSourcesConfigResponse' {} a -> s {eventSources = a} :: DescribeEventSourcesConfigResponse)

-- | The response's http status code.
describeEventSourcesConfigResponse_httpStatus :: Lens.Lens' DescribeEventSourcesConfigResponse Prelude.Int
describeEventSourcesConfigResponse_httpStatus = Lens.lens (\DescribeEventSourcesConfigResponse' {httpStatus} -> httpStatus) (\s@DescribeEventSourcesConfigResponse' {} a -> s {httpStatus = a} :: DescribeEventSourcesConfigResponse)

instance
  Prelude.NFData
    DescribeEventSourcesConfigResponse
  where
  rnf DescribeEventSourcesConfigResponse' {..} =
    Prelude.rnf eventSources
      `Prelude.seq` Prelude.rnf httpStatus
