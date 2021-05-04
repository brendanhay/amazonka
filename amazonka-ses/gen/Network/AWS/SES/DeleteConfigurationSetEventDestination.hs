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
-- Module      : Network.AWS.SES.DeleteConfigurationSetEventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SES.DeleteConfigurationSetEventDestination
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    DeleteConfigurationSetEventDestination
  where
  type
    Rs DeleteConfigurationSetEventDestination =
      DeleteConfigurationSetEventDestinationResponse
  request = Request.postQuery defaultService
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

instance
  Prelude.NFData
    DeleteConfigurationSetEventDestination

instance
  Prelude.ToHeaders
    DeleteConfigurationSetEventDestination
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteConfigurationSetEventDestination
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteConfigurationSetEventDestination
  where
  toQuery DeleteConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteConfigurationSetEventDestination" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName"
          Prelude.=: configurationSetName,
        "EventDestinationName"
          Prelude.=: eventDestinationName
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newDeleteConfigurationSetEventDestinationResponse' smart constructor.
data DeleteConfigurationSetEventDestinationResponse = DeleteConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
