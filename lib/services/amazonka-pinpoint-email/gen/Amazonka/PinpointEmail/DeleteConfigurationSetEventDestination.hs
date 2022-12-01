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
-- Module      : Amazonka.PinpointEmail.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an event destination.
--
-- In Amazon Pinpoint, /events/ include message sends, deliveries, opens,
-- clicks, bounces, and complaints. /Event destinations/ are places that
-- you can send information about these events to. For example, you can
-- send event data to Amazon SNS to receive notifications when you receive
-- bounces or complaints, or you can use Amazon Kinesis Data Firehose to
-- stream data to Amazon S3 for long-term storage.
module Amazonka.PinpointEmail.DeleteConfigurationSetEventDestination
  ( -- * Creating a Request
    DeleteConfigurationSetEventDestination (..),
    newDeleteConfigurationSetEventDestination,

    -- * Request Lenses
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestination_eventDestinationName,

    -- * Destructuring the Response
    DeleteConfigurationSetEventDestinationResponse (..),
    newDeleteConfigurationSetEventDestinationResponse,

    -- * Response Lenses
    deleteConfigurationSetEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to delete an event destination from a configuration set.
--
-- /See:/ 'newDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { -- | The name of the configuration set that contains the event destination
    -- that you want to delete.
    configurationSetName :: Prelude.Text,
    -- | The name of the event destination that you want to delete.
    eventDestinationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteConfigurationSetEventDestination_configurationSetName' - The name of the configuration set that contains the event destination
-- that you want to delete.
--
-- 'eventDestinationName', 'deleteConfigurationSetEventDestination_eventDestinationName' - The name of the event destination that you want to delete.
newDeleteConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestinationName'
  Prelude.Text ->
  DeleteConfigurationSetEventDestination
newDeleteConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestinationName_ =
    DeleteConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestinationName =
          pEventDestinationName_
      }

-- | The name of the configuration set that contains the event destination
-- that you want to delete.
deleteConfigurationSetEventDestination_configurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Prelude.Text
deleteConfigurationSetEventDestination_configurationSetName = Lens.lens (\DeleteConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetEventDestination)

-- | The name of the event destination that you want to delete.
deleteConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' DeleteConfigurationSetEventDestination Prelude.Text
deleteConfigurationSetEventDestination_eventDestinationName = Lens.lens (\DeleteConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@DeleteConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: DeleteConfigurationSetEventDestination)

instance
  Core.AWSRequest
    DeleteConfigurationSetEventDestination
  where
  type
    AWSResponse
      DeleteConfigurationSetEventDestination =
      DeleteConfigurationSetEventDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteConfigurationSetEventDestination
  where
  hashWithSalt
    _salt
    DeleteConfigurationSetEventDestination' {..} =
      _salt `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` eventDestinationName

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestination
  where
  rnf DeleteConfigurationSetEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinationName

instance
  Core.ToHeaders
    DeleteConfigurationSetEventDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    DeleteConfigurationSetEventDestination
  where
  toPath DeleteConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v1/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/event-destinations/",
        Core.toBS eventDestinationName
      ]

instance
  Core.ToQuery
    DeleteConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newDeleteConfigurationSetEventDestinationResponse' smart constructor.
data DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationSetEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfigurationSetEventDestinationResponse_httpStatus' - The response's http status code.
newDeleteConfigurationSetEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfigurationSetEventDestinationResponse
newDeleteConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    DeleteConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' DeleteConfigurationSetEventDestinationResponse Prelude.Int
deleteConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\DeleteConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: DeleteConfigurationSetEventDestinationResponse)

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestinationResponse
  where
  rnf
    DeleteConfigurationSetEventDestinationResponse' {..} =
      Prelude.rnf httpStatus
