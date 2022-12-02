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
-- Module      : Amazonka.SESV2.UpdateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the configuration of an event destination for a configuration
-- set.
--
-- /Events/ include message sends, deliveries, opens, clicks, bounces, and
-- complaints. /Event destinations/ are places that you can send
-- information about these events to. For example, you can send event data
-- to Amazon SNS to receive notifications when you receive bounces or
-- complaints, or you can use Amazon Kinesis Data Firehose to stream data
-- to Amazon S3 for long-term storage.
module Amazonka.SESV2.UpdateConfigurationSetEventDestination
  ( -- * Creating a Request
    UpdateConfigurationSetEventDestination (..),
    newUpdateConfigurationSetEventDestination,

    -- * Request Lenses
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_eventDestination,

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
import Amazonka.SESV2.Types

-- | A request to change the settings for an event destination for a
-- configuration set.
--
-- /See:/ 'newUpdateConfigurationSetEventDestination' smart constructor.
data UpdateConfigurationSetEventDestination = UpdateConfigurationSetEventDestination'
  { -- | The name of the configuration set that contains the event destination to
    -- modify.
    configurationSetName :: Prelude.Text,
    -- | The name of the event destination.
    eventDestinationName :: Prelude.Text,
    -- | An object that defines the event destination.
    eventDestination :: EventDestinationDefinition
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
-- 'configurationSetName', 'updateConfigurationSetEventDestination_configurationSetName' - The name of the configuration set that contains the event destination to
-- modify.
--
-- 'eventDestinationName', 'updateConfigurationSetEventDestination_eventDestinationName' - The name of the event destination.
--
-- 'eventDestination', 'updateConfigurationSetEventDestination_eventDestination' - An object that defines the event destination.
newUpdateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestinationName'
  Prelude.Text ->
  -- | 'eventDestination'
  EventDestinationDefinition ->
  UpdateConfigurationSetEventDestination
newUpdateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestinationName_
  pEventDestination_ =
    UpdateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestinationName =
          pEventDestinationName_,
        eventDestination =
          pEventDestination_
      }

-- | The name of the configuration set that contains the event destination to
-- modify.
updateConfigurationSetEventDestination_configurationSetName :: Lens.Lens' UpdateConfigurationSetEventDestination Prelude.Text
updateConfigurationSetEventDestination_configurationSetName = Lens.lens (\UpdateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetEventDestination)

-- | The name of the event destination.
updateConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' UpdateConfigurationSetEventDestination Prelude.Text
updateConfigurationSetEventDestination_eventDestinationName = Lens.lens (\UpdateConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@UpdateConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: UpdateConfigurationSetEventDestination)

-- | An object that defines the event destination.
updateConfigurationSetEventDestination_eventDestination :: Lens.Lens' UpdateConfigurationSetEventDestination EventDestinationDefinition
updateConfigurationSetEventDestination_eventDestination = Lens.lens (\UpdateConfigurationSetEventDestination' {eventDestination} -> eventDestination) (\s@UpdateConfigurationSetEventDestination' {} a -> s {eventDestination = a} :: UpdateConfigurationSetEventDestination)

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
      _salt `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` eventDestinationName
        `Prelude.hashWithSalt` eventDestination

instance
  Prelude.NFData
    UpdateConfigurationSetEventDestination
  where
  rnf UpdateConfigurationSetEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinationName
      `Prelude.seq` Prelude.rnf eventDestination

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
          [ Prelude.Just
              ("EventDestination" Data..= eventDestination)
          ]
      )

instance
  Data.ToPath
    UpdateConfigurationSetEventDestination
  where
  toPath UpdateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Data.toBS configurationSetName,
        "/event-destinations/",
        Data.toBS eventDestinationName
      ]

instance
  Data.ToQuery
    UpdateConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
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
