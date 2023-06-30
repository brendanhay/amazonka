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
-- Module      : Amazonka.Chime.GetEventsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for an events configuration that allows a bot to receive
-- outgoing events, such as an HTTPS endpoint or Lambda function ARN.
module Amazonka.Chime.GetEventsConfiguration
  ( -- * Creating a Request
    GetEventsConfiguration (..),
    newGetEventsConfiguration,

    -- * Request Lenses
    getEventsConfiguration_accountId,
    getEventsConfiguration_botId,

    -- * Destructuring the Response
    GetEventsConfigurationResponse (..),
    newGetEventsConfigurationResponse,

    -- * Response Lenses
    getEventsConfigurationResponse_eventsConfiguration,
    getEventsConfigurationResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventsConfiguration' smart constructor.
data GetEventsConfiguration = GetEventsConfiguration'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The bot ID.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getEventsConfiguration_accountId' - The Amazon Chime account ID.
--
-- 'botId', 'getEventsConfiguration_botId' - The bot ID.
newGetEventsConfiguration ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  GetEventsConfiguration
newGetEventsConfiguration pAccountId_ pBotId_ =
  GetEventsConfiguration'
    { accountId = pAccountId_,
      botId = pBotId_
    }

-- | The Amazon Chime account ID.
getEventsConfiguration_accountId :: Lens.Lens' GetEventsConfiguration Prelude.Text
getEventsConfiguration_accountId = Lens.lens (\GetEventsConfiguration' {accountId} -> accountId) (\s@GetEventsConfiguration' {} a -> s {accountId = a} :: GetEventsConfiguration)

-- | The bot ID.
getEventsConfiguration_botId :: Lens.Lens' GetEventsConfiguration Prelude.Text
getEventsConfiguration_botId = Lens.lens (\GetEventsConfiguration' {botId} -> botId) (\s@GetEventsConfiguration' {} a -> s {botId = a} :: GetEventsConfiguration)

instance Core.AWSRequest GetEventsConfiguration where
  type
    AWSResponse GetEventsConfiguration =
      GetEventsConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventsConfigurationResponse'
            Prelude.<$> (x Data..?> "EventsConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventsConfiguration where
  hashWithSalt _salt GetEventsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData GetEventsConfiguration where
  rnf GetEventsConfiguration' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders GetEventsConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetEventsConfiguration where
  toPath GetEventsConfiguration' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/bots/",
        Data.toBS botId,
        "/events-configuration"
      ]

instance Data.ToQuery GetEventsConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventsConfigurationResponse' smart constructor.
data GetEventsConfigurationResponse = GetEventsConfigurationResponse'
  { -- | The events configuration details.
    eventsConfiguration :: Prelude.Maybe EventsConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventsConfiguration', 'getEventsConfigurationResponse_eventsConfiguration' - The events configuration details.
--
-- 'httpStatus', 'getEventsConfigurationResponse_httpStatus' - The response's http status code.
newGetEventsConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventsConfigurationResponse
newGetEventsConfigurationResponse pHttpStatus_ =
  GetEventsConfigurationResponse'
    { eventsConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The events configuration details.
getEventsConfigurationResponse_eventsConfiguration :: Lens.Lens' GetEventsConfigurationResponse (Prelude.Maybe EventsConfiguration)
getEventsConfigurationResponse_eventsConfiguration = Lens.lens (\GetEventsConfigurationResponse' {eventsConfiguration} -> eventsConfiguration) (\s@GetEventsConfigurationResponse' {} a -> s {eventsConfiguration = a} :: GetEventsConfigurationResponse)

-- | The response's http status code.
getEventsConfigurationResponse_httpStatus :: Lens.Lens' GetEventsConfigurationResponse Prelude.Int
getEventsConfigurationResponse_httpStatus = Lens.lens (\GetEventsConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetEventsConfigurationResponse' {} a -> s {httpStatus = a} :: GetEventsConfigurationResponse)

instance
  Prelude.NFData
    GetEventsConfigurationResponse
  where
  rnf GetEventsConfigurationResponse' {..} =
    Prelude.rnf eventsConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
