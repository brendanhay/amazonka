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
-- Module      : Amazonka.IoT.UpdateEventConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event configurations.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateEventConfigurations>
-- action.
module Amazonka.IoT.UpdateEventConfigurations
  ( -- * Creating a Request
    UpdateEventConfigurations (..),
    newUpdateEventConfigurations,

    -- * Request Lenses
    updateEventConfigurations_eventConfigurations,

    -- * Destructuring the Response
    UpdateEventConfigurationsResponse (..),
    newUpdateEventConfigurationsResponse,

    -- * Response Lenses
    updateEventConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventConfigurations' smart constructor.
data UpdateEventConfigurations = UpdateEventConfigurations'
  { -- | The new event configuration values.
    eventConfigurations :: Prelude.Maybe (Prelude.HashMap EventType Configuration)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventConfigurations', 'updateEventConfigurations_eventConfigurations' - The new event configuration values.
newUpdateEventConfigurations ::
  UpdateEventConfigurations
newUpdateEventConfigurations =
  UpdateEventConfigurations'
    { eventConfigurations =
        Prelude.Nothing
    }

-- | The new event configuration values.
updateEventConfigurations_eventConfigurations :: Lens.Lens' UpdateEventConfigurations (Prelude.Maybe (Prelude.HashMap EventType Configuration))
updateEventConfigurations_eventConfigurations = Lens.lens (\UpdateEventConfigurations' {eventConfigurations} -> eventConfigurations) (\s@UpdateEventConfigurations' {} a -> s {eventConfigurations = a} :: UpdateEventConfigurations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest UpdateEventConfigurations where
  type
    AWSResponse UpdateEventConfigurations =
      UpdateEventConfigurationsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventConfigurationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventConfigurations where
  hashWithSalt _salt UpdateEventConfigurations' {..} =
    _salt `Prelude.hashWithSalt` eventConfigurations

instance Prelude.NFData UpdateEventConfigurations where
  rnf UpdateEventConfigurations' {..} =
    Prelude.rnf eventConfigurations

instance Core.ToHeaders UpdateEventConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateEventConfigurations where
  toJSON UpdateEventConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("eventConfigurations" Core..=)
              Prelude.<$> eventConfigurations
          ]
      )

instance Core.ToPath UpdateEventConfigurations where
  toPath = Prelude.const "/event-configurations"

instance Core.ToQuery UpdateEventConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventConfigurationsResponse' smart constructor.
data UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEventConfigurationsResponse_httpStatus' - The response's http status code.
newUpdateEventConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventConfigurationsResponse
newUpdateEventConfigurationsResponse pHttpStatus_ =
  UpdateEventConfigurationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEventConfigurationsResponse_httpStatus :: Lens.Lens' UpdateEventConfigurationsResponse Prelude.Int
updateEventConfigurationsResponse_httpStatus = Lens.lens (\UpdateEventConfigurationsResponse' {httpStatus} -> httpStatus) (\s@UpdateEventConfigurationsResponse' {} a -> s {httpStatus = a} :: UpdateEventConfigurationsResponse)

instance
  Prelude.NFData
    UpdateEventConfigurationsResponse
  where
  rnf UpdateEventConfigurationsResponse' {..} =
    Prelude.rnf httpStatus
