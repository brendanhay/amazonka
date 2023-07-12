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
-- Module      : Amazonka.ConnectCases.PutCaseEventConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API for adding case event publishing configuration
module Amazonka.ConnectCases.PutCaseEventConfiguration
  ( -- * Creating a Request
    PutCaseEventConfiguration (..),
    newPutCaseEventConfiguration,

    -- * Request Lenses
    putCaseEventConfiguration_domainId,
    putCaseEventConfiguration_eventBridge,

    -- * Destructuring the Response
    PutCaseEventConfigurationResponse (..),
    newPutCaseEventConfigurationResponse,

    -- * Response Lenses
    putCaseEventConfigurationResponse_httpStatus,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutCaseEventConfiguration' smart constructor.
data PutCaseEventConfiguration = PutCaseEventConfiguration'
  { -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | Configuration to enable EventBridge case event delivery and determine
    -- what data is delivered.
    eventBridge :: EventBridgeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCaseEventConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'putCaseEventConfiguration_domainId' - The unique identifier of the Cases domain.
--
-- 'eventBridge', 'putCaseEventConfiguration_eventBridge' - Configuration to enable EventBridge case event delivery and determine
-- what data is delivered.
newPutCaseEventConfiguration ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'eventBridge'
  EventBridgeConfiguration ->
  PutCaseEventConfiguration
newPutCaseEventConfiguration pDomainId_ pEventBridge_ =
  PutCaseEventConfiguration'
    { domainId = pDomainId_,
      eventBridge = pEventBridge_
    }

-- | The unique identifier of the Cases domain.
putCaseEventConfiguration_domainId :: Lens.Lens' PutCaseEventConfiguration Prelude.Text
putCaseEventConfiguration_domainId = Lens.lens (\PutCaseEventConfiguration' {domainId} -> domainId) (\s@PutCaseEventConfiguration' {} a -> s {domainId = a} :: PutCaseEventConfiguration)

-- | Configuration to enable EventBridge case event delivery and determine
-- what data is delivered.
putCaseEventConfiguration_eventBridge :: Lens.Lens' PutCaseEventConfiguration EventBridgeConfiguration
putCaseEventConfiguration_eventBridge = Lens.lens (\PutCaseEventConfiguration' {eventBridge} -> eventBridge) (\s@PutCaseEventConfiguration' {} a -> s {eventBridge = a} :: PutCaseEventConfiguration)

instance Core.AWSRequest PutCaseEventConfiguration where
  type
    AWSResponse PutCaseEventConfiguration =
      PutCaseEventConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutCaseEventConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutCaseEventConfiguration where
  hashWithSalt _salt PutCaseEventConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` eventBridge

instance Prelude.NFData PutCaseEventConfiguration where
  rnf PutCaseEventConfiguration' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf eventBridge

instance Data.ToHeaders PutCaseEventConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutCaseEventConfiguration where
  toJSON PutCaseEventConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("eventBridge" Data..= eventBridge)]
      )

instance Data.ToPath PutCaseEventConfiguration where
  toPath PutCaseEventConfiguration' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/case-event-configuration"
      ]

instance Data.ToQuery PutCaseEventConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutCaseEventConfigurationResponse' smart constructor.
data PutCaseEventConfigurationResponse = PutCaseEventConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutCaseEventConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putCaseEventConfigurationResponse_httpStatus' - The response's http status code.
newPutCaseEventConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutCaseEventConfigurationResponse
newPutCaseEventConfigurationResponse pHttpStatus_ =
  PutCaseEventConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putCaseEventConfigurationResponse_httpStatus :: Lens.Lens' PutCaseEventConfigurationResponse Prelude.Int
putCaseEventConfigurationResponse_httpStatus = Lens.lens (\PutCaseEventConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutCaseEventConfigurationResponse' {} a -> s {httpStatus = a} :: PutCaseEventConfigurationResponse)

instance
  Prelude.NFData
    PutCaseEventConfigurationResponse
  where
  rnf PutCaseEventConfigurationResponse' {..} =
    Prelude.rnf httpStatus
