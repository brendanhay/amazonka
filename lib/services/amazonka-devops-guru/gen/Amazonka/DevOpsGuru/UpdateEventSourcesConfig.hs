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
-- Module      : Amazonka.DevOpsGuru.UpdateEventSourcesConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables integration with a service that can be integrated
-- with DevOps Guru. The one service that can be integrated with DevOps
-- Guru is Amazon CodeGuru Profiler, which can produce proactive
-- recommendations which can be stored and viewed in DevOps Guru.
module Amazonka.DevOpsGuru.UpdateEventSourcesConfig
  ( -- * Creating a Request
    UpdateEventSourcesConfig (..),
    newUpdateEventSourcesConfig,

    -- * Request Lenses
    updateEventSourcesConfig_eventSources,

    -- * Destructuring the Response
    UpdateEventSourcesConfigResponse (..),
    newUpdateEventSourcesConfigResponse,

    -- * Response Lenses
    updateEventSourcesConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventSourcesConfig' smart constructor.
data UpdateEventSourcesConfig = UpdateEventSourcesConfig'
  { -- | Configuration information about the integration of DevOps Guru as the
    -- Consumer via EventBridge with another AWS Service.
    eventSources :: Prelude.Maybe EventSourcesConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventSourcesConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSources', 'updateEventSourcesConfig_eventSources' - Configuration information about the integration of DevOps Guru as the
-- Consumer via EventBridge with another AWS Service.
newUpdateEventSourcesConfig ::
  UpdateEventSourcesConfig
newUpdateEventSourcesConfig =
  UpdateEventSourcesConfig'
    { eventSources =
        Prelude.Nothing
    }

-- | Configuration information about the integration of DevOps Guru as the
-- Consumer via EventBridge with another AWS Service.
updateEventSourcesConfig_eventSources :: Lens.Lens' UpdateEventSourcesConfig (Prelude.Maybe EventSourcesConfig)
updateEventSourcesConfig_eventSources = Lens.lens (\UpdateEventSourcesConfig' {eventSources} -> eventSources) (\s@UpdateEventSourcesConfig' {} a -> s {eventSources = a} :: UpdateEventSourcesConfig)

instance Core.AWSRequest UpdateEventSourcesConfig where
  type
    AWSResponse UpdateEventSourcesConfig =
      UpdateEventSourcesConfigResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventSourcesConfigResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventSourcesConfig where
  hashWithSalt _salt UpdateEventSourcesConfig' {..} =
    _salt `Prelude.hashWithSalt` eventSources

instance Prelude.NFData UpdateEventSourcesConfig where
  rnf UpdateEventSourcesConfig' {..} =
    Prelude.rnf eventSources

instance Core.ToHeaders UpdateEventSourcesConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEventSourcesConfig where
  toJSON UpdateEventSourcesConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("EventSources" Core..=) Prelude.<$> eventSources]
      )

instance Core.ToPath UpdateEventSourcesConfig where
  toPath = Prelude.const "/event-sources"

instance Core.ToQuery UpdateEventSourcesConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventSourcesConfigResponse' smart constructor.
data UpdateEventSourcesConfigResponse = UpdateEventSourcesConfigResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventSourcesConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEventSourcesConfigResponse_httpStatus' - The response's http status code.
newUpdateEventSourcesConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventSourcesConfigResponse
newUpdateEventSourcesConfigResponse pHttpStatus_ =
  UpdateEventSourcesConfigResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEventSourcesConfigResponse_httpStatus :: Lens.Lens' UpdateEventSourcesConfigResponse Prelude.Int
updateEventSourcesConfigResponse_httpStatus = Lens.lens (\UpdateEventSourcesConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateEventSourcesConfigResponse' {} a -> s {httpStatus = a} :: UpdateEventSourcesConfigResponse)

instance
  Prelude.NFData
    UpdateEventSourcesConfigResponse
  where
  rnf UpdateEventSourcesConfigResponse' {..} =
    Prelude.rnf httpStatus
