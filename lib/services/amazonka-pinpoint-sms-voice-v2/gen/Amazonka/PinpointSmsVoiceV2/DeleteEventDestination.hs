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
-- Module      : Amazonka.PinpointSmsVoiceV2.DeleteEventDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing event destination.
--
-- An event destination is a location where you send response information
-- about the messages that you send. For example, when a message is
-- delivered successfully, you can send information about that event to an
-- Amazon CloudWatch destination, or send notifications to endpoints that
-- are subscribed to an Amazon SNS topic.
module Amazonka.PinpointSmsVoiceV2.DeleteEventDestination
  ( -- * Creating a Request
    DeleteEventDestination (..),
    newDeleteEventDestination,

    -- * Request Lenses
    deleteEventDestination_configurationSetName,
    deleteEventDestination_eventDestinationName,

    -- * Destructuring the Response
    DeleteEventDestinationResponse (..),
    newDeleteEventDestinationResponse,

    -- * Response Lenses
    deleteEventDestinationResponse_configurationSetName,
    deleteEventDestinationResponse_eventDestination,
    deleteEventDestinationResponse_configurationSetArn,
    deleteEventDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventDestination' smart constructor.
data DeleteEventDestination = DeleteEventDestination'
  { -- | The name of the configuration set or the configuration set\'s Amazon
    -- Resource Name (ARN) to remove the event destination from. The
    -- ConfigurateSetName and ConfigurationSetArn can be found using the
    -- DescribeConfigurationSets action.
    configurationSetName :: Prelude.Text,
    -- | The name of the event destination to delete.
    eventDestinationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteEventDestination_configurationSetName' - The name of the configuration set or the configuration set\'s Amazon
-- Resource Name (ARN) to remove the event destination from. The
-- ConfigurateSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
--
-- 'eventDestinationName', 'deleteEventDestination_eventDestinationName' - The name of the event destination to delete.
newDeleteEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestinationName'
  Prelude.Text ->
  DeleteEventDestination
newDeleteEventDestination
  pConfigurationSetName_
  pEventDestinationName_ =
    DeleteEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestinationName = pEventDestinationName_
      }

-- | The name of the configuration set or the configuration set\'s Amazon
-- Resource Name (ARN) to remove the event destination from. The
-- ConfigurateSetName and ConfigurationSetArn can be found using the
-- DescribeConfigurationSets action.
deleteEventDestination_configurationSetName :: Lens.Lens' DeleteEventDestination Prelude.Text
deleteEventDestination_configurationSetName = Lens.lens (\DeleteEventDestination' {configurationSetName} -> configurationSetName) (\s@DeleteEventDestination' {} a -> s {configurationSetName = a} :: DeleteEventDestination)

-- | The name of the event destination to delete.
deleteEventDestination_eventDestinationName :: Lens.Lens' DeleteEventDestination Prelude.Text
deleteEventDestination_eventDestinationName = Lens.lens (\DeleteEventDestination' {eventDestinationName} -> eventDestinationName) (\s@DeleteEventDestination' {} a -> s {eventDestinationName = a} :: DeleteEventDestination)

instance Core.AWSRequest DeleteEventDestination where
  type
    AWSResponse DeleteEventDestination =
      DeleteEventDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEventDestinationResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "EventDestination")
            Prelude.<*> (x Data..?> "ConfigurationSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventDestination where
  hashWithSalt _salt DeleteEventDestination' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` eventDestinationName

instance Prelude.NFData DeleteEventDestination where
  rnf DeleteEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestinationName

instance Data.ToHeaders DeleteEventDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PinpointSMSVoiceV2.DeleteEventDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEventDestination where
  toJSON DeleteEventDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              ),
            Prelude.Just
              ( "EventDestinationName"
                  Data..= eventDestinationName
              )
          ]
      )

instance Data.ToPath DeleteEventDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEventDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventDestinationResponse' smart constructor.
data DeleteEventDestinationResponse = DeleteEventDestinationResponse'
  { -- | The name of the configuration set the event destination was deleted
    -- from.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | The event destination object that was deleted.
    eventDestination :: Prelude.Maybe EventDestination,
    -- | The Amazon Resource Name (ARN) of the configuration set.
    configurationSetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'deleteEventDestinationResponse_configurationSetName' - The name of the configuration set the event destination was deleted
-- from.
--
-- 'eventDestination', 'deleteEventDestinationResponse_eventDestination' - The event destination object that was deleted.
--
-- 'configurationSetArn', 'deleteEventDestinationResponse_configurationSetArn' - The Amazon Resource Name (ARN) of the configuration set.
--
-- 'httpStatus', 'deleteEventDestinationResponse_httpStatus' - The response's http status code.
newDeleteEventDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventDestinationResponse
newDeleteEventDestinationResponse pHttpStatus_ =
  DeleteEventDestinationResponse'
    { configurationSetName =
        Prelude.Nothing,
      eventDestination = Prelude.Nothing,
      configurationSetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the configuration set the event destination was deleted
-- from.
deleteEventDestinationResponse_configurationSetName :: Lens.Lens' DeleteEventDestinationResponse (Prelude.Maybe Prelude.Text)
deleteEventDestinationResponse_configurationSetName = Lens.lens (\DeleteEventDestinationResponse' {configurationSetName} -> configurationSetName) (\s@DeleteEventDestinationResponse' {} a -> s {configurationSetName = a} :: DeleteEventDestinationResponse)

-- | The event destination object that was deleted.
deleteEventDestinationResponse_eventDestination :: Lens.Lens' DeleteEventDestinationResponse (Prelude.Maybe EventDestination)
deleteEventDestinationResponse_eventDestination = Lens.lens (\DeleteEventDestinationResponse' {eventDestination} -> eventDestination) (\s@DeleteEventDestinationResponse' {} a -> s {eventDestination = a} :: DeleteEventDestinationResponse)

-- | The Amazon Resource Name (ARN) of the configuration set.
deleteEventDestinationResponse_configurationSetArn :: Lens.Lens' DeleteEventDestinationResponse (Prelude.Maybe Prelude.Text)
deleteEventDestinationResponse_configurationSetArn = Lens.lens (\DeleteEventDestinationResponse' {configurationSetArn} -> configurationSetArn) (\s@DeleteEventDestinationResponse' {} a -> s {configurationSetArn = a} :: DeleteEventDestinationResponse)

-- | The response's http status code.
deleteEventDestinationResponse_httpStatus :: Lens.Lens' DeleteEventDestinationResponse Prelude.Int
deleteEventDestinationResponse_httpStatus = Lens.lens (\DeleteEventDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteEventDestinationResponse' {} a -> s {httpStatus = a} :: DeleteEventDestinationResponse)

instance
  Prelude.NFData
    DeleteEventDestinationResponse
  where
  rnf DeleteEventDestinationResponse' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestination
      `Prelude.seq` Prelude.rnf configurationSetArn
      `Prelude.seq` Prelude.rnf httpStatus
