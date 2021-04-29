{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.UpdateEventConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event configurations.
module Network.AWS.IoT.UpdateEventConfigurations
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEventConfigurations' smart constructor.
data UpdateEventConfigurations = UpdateEventConfigurations'
  { -- | The new event configuration values.
    eventConfigurations :: Prelude.Maybe (Prelude.HashMap EventType Configuration)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateEventConfigurations_eventConfigurations = Lens.lens (\UpdateEventConfigurations' {eventConfigurations} -> eventConfigurations) (\s@UpdateEventConfigurations' {} a -> s {eventConfigurations = a} :: UpdateEventConfigurations) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest UpdateEventConfigurations where
  type
    Rs UpdateEventConfigurations =
      UpdateEventConfigurationsResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventConfigurationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventConfigurations

instance Prelude.NFData UpdateEventConfigurations

instance Prelude.ToHeaders UpdateEventConfigurations where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateEventConfigurations where
  toJSON UpdateEventConfigurations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("eventConfigurations" Prelude..=)
              Prelude.<$> eventConfigurations
          ]
      )

instance Prelude.ToPath UpdateEventConfigurations where
  toPath = Prelude.const "/event-configurations"

instance Prelude.ToQuery UpdateEventConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventConfigurationsResponse' smart constructor.
data UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
