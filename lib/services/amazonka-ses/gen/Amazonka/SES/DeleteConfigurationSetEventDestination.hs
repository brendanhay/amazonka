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
-- Module      : Amazonka.SES.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration set event destination. Configuration set event
-- destinations are associated with configuration sets, which enable you to
-- publish email sending events. For information about using configuration
-- sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.DeleteConfigurationSetEventDestination
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to delete a configuration set event destination.
-- Configuration set event destinations are associated with configuration
-- sets, which enable you to publish email sending events. For information
-- about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newDeleteConfigurationSetEventDestination' smart constructor.
data DeleteConfigurationSetEventDestination = DeleteConfigurationSetEventDestination'
  { -- | The name of the configuration set from which to delete the event
    -- destination.
    configurationSetName :: Prelude.Text,
    -- | The name of the event destination to delete.
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
-- 'configurationSetName', 'deleteConfigurationSetEventDestination_configurationSetName' - The name of the configuration set from which to delete the event
-- destination.
--
-- 'eventDestinationName', 'deleteConfigurationSetEventDestination_eventDestinationName' - The name of the event destination to delete.
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

-- | The name of the configuration set from which to delete the event
-- destination.
deleteConfigurationSetEventDestination_configurationSetName :: Lens.Lens' DeleteConfigurationSetEventDestination Prelude.Text
deleteConfigurationSetEventDestination_configurationSetName = Lens.lens (\DeleteConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@DeleteConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: DeleteConfigurationSetEventDestination)

-- | The name of the event destination to delete.
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
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteConfigurationSetEventDestinationResult"
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
      _salt
        `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` eventDestinationName

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestination
  where
  rnf DeleteConfigurationSetEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinationName

instance
  Data.ToHeaders
    DeleteConfigurationSetEventDestination
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteConfigurationSetEventDestination
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteConfigurationSetEventDestination
  where
  toQuery DeleteConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteConfigurationSetEventDestination" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Data.=: configurationSetName,
        "EventDestinationName" Data.=: eventDestinationName
      ]

-- | An empty element returned on a successful request.
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
