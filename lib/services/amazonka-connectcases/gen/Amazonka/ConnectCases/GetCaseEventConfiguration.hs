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
-- Module      : Amazonka.ConnectCases.GetCaseEventConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the case event publishing configuration.
module Amazonka.ConnectCases.GetCaseEventConfiguration
  ( -- * Creating a Request
    GetCaseEventConfiguration (..),
    newGetCaseEventConfiguration,

    -- * Request Lenses
    getCaseEventConfiguration_domainId,

    -- * Destructuring the Response
    GetCaseEventConfigurationResponse (..),
    newGetCaseEventConfigurationResponse,

    -- * Response Lenses
    getCaseEventConfigurationResponse_httpStatus,
    getCaseEventConfigurationResponse_eventBridge,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCaseEventConfiguration' smart constructor.
data GetCaseEventConfiguration = GetCaseEventConfiguration'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCaseEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'getCaseEventConfiguration_domainId' - The unique identifier of the Cases domain.
newGetCaseEventConfiguration ::
  -- | 'domainId'
  Prelude.Text ->
  GetCaseEventConfiguration
newGetCaseEventConfiguration pDomainId_ =
  GetCaseEventConfiguration' {domainId = pDomainId_}

-- | The unique identifier of the Cases domain.
getCaseEventConfiguration_domainId :: Lens.Lens' GetCaseEventConfiguration Prelude.Text
getCaseEventConfiguration_domainId = Lens.lens (\GetCaseEventConfiguration' {domainId} -> domainId) (\s@GetCaseEventConfiguration' {} a -> s {domainId = a} :: GetCaseEventConfiguration)

instance Core.AWSRequest GetCaseEventConfiguration where
  type
    AWSResponse GetCaseEventConfiguration =
      GetCaseEventConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCaseEventConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "eventBridge")
      )

instance Prelude.Hashable GetCaseEventConfiguration where
  hashWithSalt _salt GetCaseEventConfiguration' {..} =
    _salt `Prelude.hashWithSalt` domainId

instance Prelude.NFData GetCaseEventConfiguration where
  rnf GetCaseEventConfiguration' {..} =
    Prelude.rnf domainId

instance Data.ToHeaders GetCaseEventConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCaseEventConfiguration where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetCaseEventConfiguration where
  toPath GetCaseEventConfiguration' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/case-event-configuration"
      ]

instance Data.ToQuery GetCaseEventConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCaseEventConfigurationResponse' smart constructor.
data GetCaseEventConfigurationResponse = GetCaseEventConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Configuration to enable EventBridge case event delivery and determine
    -- what data is delivered.
    eventBridge :: EventBridgeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCaseEventConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCaseEventConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'eventBridge', 'getCaseEventConfigurationResponse_eventBridge' - Configuration to enable EventBridge case event delivery and determine
-- what data is delivered.
newGetCaseEventConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'eventBridge'
  EventBridgeConfiguration ->
  GetCaseEventConfigurationResponse
newGetCaseEventConfigurationResponse
  pHttpStatus_
  pEventBridge_ =
    GetCaseEventConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        eventBridge = pEventBridge_
      }

-- | The response's http status code.
getCaseEventConfigurationResponse_httpStatus :: Lens.Lens' GetCaseEventConfigurationResponse Prelude.Int
getCaseEventConfigurationResponse_httpStatus = Lens.lens (\GetCaseEventConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetCaseEventConfigurationResponse' {} a -> s {httpStatus = a} :: GetCaseEventConfigurationResponse)

-- | Configuration to enable EventBridge case event delivery and determine
-- what data is delivered.
getCaseEventConfigurationResponse_eventBridge :: Lens.Lens' GetCaseEventConfigurationResponse EventBridgeConfiguration
getCaseEventConfigurationResponse_eventBridge = Lens.lens (\GetCaseEventConfigurationResponse' {eventBridge} -> eventBridge) (\s@GetCaseEventConfigurationResponse' {} a -> s {eventBridge = a} :: GetCaseEventConfigurationResponse)

instance
  Prelude.NFData
    GetCaseEventConfigurationResponse
  where
  rnf GetCaseEventConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf eventBridge
