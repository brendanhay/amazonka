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
-- Module      : Amazonka.SmsVoice.UpdateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an event destination in a configuration set. An event destination
-- is a location that you publish information about your voice calls to.
-- For example, you can log an event to an Amazon CloudWatch destination
-- when a call fails.
module Amazonka.SmsVoice.UpdateConfigurationSetEventDestination
  ( -- * Creating a Request
    UpdateConfigurationSetEventDestination (..),
    newUpdateConfigurationSetEventDestination,

    -- * Request Lenses
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_configurationSetName,

    -- * Destructuring the Response
    UpdateConfigurationSetEventDestinationResponse (..),
    newUpdateConfigurationSetEventDestinationResponse,

    -- * Response Lenses
    updateConfigurationSetEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SmsVoice.Types

-- | UpdateConfigurationSetEventDestinationRequest
--
-- /See:/ 'newUpdateConfigurationSetEventDestination' smart constructor.
data UpdateConfigurationSetEventDestination = UpdateConfigurationSetEventDestination'
  { eventDestination :: Prelude.Maybe EventDestinationDefinition,
    -- | EventDestinationName
    eventDestinationName :: Prelude.Text,
    -- | ConfigurationSetName
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDestination', 'updateConfigurationSetEventDestination_eventDestination' - Undocumented member.
--
-- 'eventDestinationName', 'updateConfigurationSetEventDestination_eventDestinationName' - EventDestinationName
--
-- 'configurationSetName', 'updateConfigurationSetEventDestination_configurationSetName' - ConfigurationSetName
newUpdateConfigurationSetEventDestination ::
  -- | 'eventDestinationName'
  Prelude.Text ->
  -- | 'configurationSetName'
  Prelude.Text ->
  UpdateConfigurationSetEventDestination
newUpdateConfigurationSetEventDestination
  pEventDestinationName_
  pConfigurationSetName_ =
    UpdateConfigurationSetEventDestination'
      { eventDestination =
          Prelude.Nothing,
        eventDestinationName =
          pEventDestinationName_,
        configurationSetName =
          pConfigurationSetName_
      }

-- | Undocumented member.
updateConfigurationSetEventDestination_eventDestination :: Lens.Lens' UpdateConfigurationSetEventDestination (Prelude.Maybe EventDestinationDefinition)
updateConfigurationSetEventDestination_eventDestination = Lens.lens (\UpdateConfigurationSetEventDestination' {eventDestination} -> eventDestination) (\s@UpdateConfigurationSetEventDestination' {} a -> s {eventDestination = a} :: UpdateConfigurationSetEventDestination)

-- | EventDestinationName
updateConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' UpdateConfigurationSetEventDestination Prelude.Text
updateConfigurationSetEventDestination_eventDestinationName = Lens.lens (\UpdateConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@UpdateConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: UpdateConfigurationSetEventDestination)

-- | ConfigurationSetName
updateConfigurationSetEventDestination_configurationSetName :: Lens.Lens' UpdateConfigurationSetEventDestination Prelude.Text
updateConfigurationSetEventDestination_configurationSetName = Lens.lens (\UpdateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetEventDestination)

instance
  Core.AWSRequest
    UpdateConfigurationSetEventDestination
  where
  type
    AWSResponse
      UpdateConfigurationSetEventDestination =
      UpdateConfigurationSetEventDestinationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateConfigurationSetEventDestination
  where
  hashWithSalt
    _salt
    UpdateConfigurationSetEventDestination' {..} =
      _salt
        `Prelude.hashWithSalt` eventDestination
        `Prelude.hashWithSalt` eventDestinationName
        `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    UpdateConfigurationSetEventDestination
  where
  rnf UpdateConfigurationSetEventDestination' {..} =
    Prelude.rnf eventDestination
      `Prelude.seq` Prelude.rnf eventDestinationName
      `Prelude.seq` Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    UpdateConfigurationSetEventDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateConfigurationSetEventDestination
  where
  toJSON UpdateConfigurationSetEventDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventDestination" Data..=)
              Prelude.<$> eventDestination
          ]
      )

instance
  Data.ToPath
    UpdateConfigurationSetEventDestination
  where
  toPath UpdateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v1/sms-voice/configuration-sets/",
        Data.toBS configurationSetName,
        "/event-destinations/",
        Data.toBS eventDestinationName
      ]

instance
  Data.ToQuery
    UpdateConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An empty object that indicates that the event destination was updated
-- successfully.
--
-- /See:/ 'newUpdateConfigurationSetEventDestinationResponse' smart constructor.
data UpdateConfigurationSetEventDestinationResponse = UpdateConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationSetEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfigurationSetEventDestinationResponse_httpStatus' - The response's http status code.
newUpdateConfigurationSetEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConfigurationSetEventDestinationResponse
newUpdateConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    UpdateConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' UpdateConfigurationSetEventDestinationResponse Prelude.Int
updateConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\UpdateConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@UpdateConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: UpdateConfigurationSetEventDestinationResponse)

instance
  Prelude.NFData
    UpdateConfigurationSetEventDestinationResponse
  where
  rnf
    UpdateConfigurationSetEventDestinationResponse' {..} =
      Prelude.rnf httpStatus
