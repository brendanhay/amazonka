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
-- Module      : Amazonka.SES.UpdateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event destination of a configuration set. Event destinations
-- are associated with configuration sets, which enable you to publish
-- email sending events to Amazon CloudWatch, Amazon Kinesis Firehose, or
-- Amazon Simple Notification Service (Amazon SNS). For information about
-- using configuration sets, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Monitoring Your Amazon SES Sending Activity>
-- in the /Amazon SES Developer Guide./
--
-- When you create or update an event destination, you must provide one,
-- and only one, destination. The destination can be Amazon CloudWatch,
-- Amazon Kinesis Firehose, or Amazon Simple Notification Service (Amazon
-- SNS).
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.UpdateConfigurationSetEventDestination
  ( -- * Creating a Request
    UpdateConfigurationSetEventDestination (..),
    newUpdateConfigurationSetEventDestination,

    -- * Request Lenses
    updateConfigurationSetEventDestination_configurationSetName,
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
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to update the event destination of a configuration
-- set. Configuration sets enable you to publish email sending events. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newUpdateConfigurationSetEventDestination' smart constructor.
data UpdateConfigurationSetEventDestination = UpdateConfigurationSetEventDestination'
  { -- | The name of the configuration set that contains the event destination
    -- that you want to update.
    configurationSetName :: Prelude.Text,
    -- | The event destination object that you want to apply to the specified
    -- configuration set.
    eventDestination :: EventDestination
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
-- 'configurationSetName', 'updateConfigurationSetEventDestination_configurationSetName' - The name of the configuration set that contains the event destination
-- that you want to update.
--
-- 'eventDestination', 'updateConfigurationSetEventDestination_eventDestination' - The event destination object that you want to apply to the specified
-- configuration set.
newUpdateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestination'
  EventDestination ->
  UpdateConfigurationSetEventDestination
newUpdateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestination_ =
    UpdateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestination =
          pEventDestination_
      }

-- | The name of the configuration set that contains the event destination
-- that you want to update.
updateConfigurationSetEventDestination_configurationSetName :: Lens.Lens' UpdateConfigurationSetEventDestination Prelude.Text
updateConfigurationSetEventDestination_configurationSetName = Lens.lens (\UpdateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@UpdateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: UpdateConfigurationSetEventDestination)

-- | The event destination object that you want to apply to the specified
-- configuration set.
updateConfigurationSetEventDestination_eventDestination :: Lens.Lens' UpdateConfigurationSetEventDestination EventDestination
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
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdateConfigurationSetEventDestinationResult"
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
        `Prelude.hashWithSalt` eventDestination

instance
  Prelude.NFData
    UpdateConfigurationSetEventDestination
  where
  rnf UpdateConfigurationSetEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestination

instance
  Core.ToHeaders
    UpdateConfigurationSetEventDestination
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    UpdateConfigurationSetEventDestination
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateConfigurationSetEventDestination
  where
  toQuery UpdateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "UpdateConfigurationSetEventDestination" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName,
        "EventDestination" Core.=: eventDestination
      ]

-- | An empty element returned on a successful request.
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
