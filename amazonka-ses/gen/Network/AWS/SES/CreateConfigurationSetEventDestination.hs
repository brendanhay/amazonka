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
-- Module      : Network.AWS.SES.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration set event destination.
--
-- When you create or update an event destination, you must provide one,
-- and only one, destination. The destination can be CloudWatch, Amazon
-- Kinesis Firehose, or Amazon Simple Notification Service (Amazon SNS).
--
-- An event destination is the AWS service to which Amazon SES publishes
-- the email sending events associated with a configuration set. For
-- information about using configuration sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.CreateConfigurationSetEventDestination
  ( -- * Creating a Request
    CreateConfigurationSetEventDestination (..),
    newCreateConfigurationSetEventDestination,

    -- * Request Lenses
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestination,

    -- * Destructuring the Response
    CreateConfigurationSetEventDestinationResponse (..),
    newCreateConfigurationSetEventDestinationResponse,

    -- * Response Lenses
    createConfigurationSetEventDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | Represents a request to create a configuration set event destination. A
-- configuration set event destination, which can be either Amazon
-- CloudWatch or Amazon Kinesis Firehose, describes an AWS service in which
-- Amazon SES publishes the email sending events associated with a
-- configuration set. For information about using configuration sets, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide>.
--
-- /See:/ 'newCreateConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { -- | The name of the configuration set that the event destination should be
    -- associated with.
    configurationSetName :: Core.Text,
    -- | An object that describes the AWS service that email sending event
    -- information will be published to.
    eventDestination :: EventDestination
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConfigurationSetEventDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'createConfigurationSetEventDestination_configurationSetName' - The name of the configuration set that the event destination should be
-- associated with.
--
-- 'eventDestination', 'createConfigurationSetEventDestination_eventDestination' - An object that describes the AWS service that email sending event
-- information will be published to.
newCreateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Core.Text ->
  -- | 'eventDestination'
  EventDestination ->
  CreateConfigurationSetEventDestination
newCreateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestination_ =
    CreateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestination =
          pEventDestination_
      }

-- | The name of the configuration set that the event destination should be
-- associated with.
createConfigurationSetEventDestination_configurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Core.Text
createConfigurationSetEventDestination_configurationSetName = Lens.lens (\CreateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: CreateConfigurationSetEventDestination)

-- | An object that describes the AWS service that email sending event
-- information will be published to.
createConfigurationSetEventDestination_eventDestination :: Lens.Lens' CreateConfigurationSetEventDestination EventDestination
createConfigurationSetEventDestination_eventDestination = Lens.lens (\CreateConfigurationSetEventDestination' {eventDestination} -> eventDestination) (\s@CreateConfigurationSetEventDestination' {} a -> s {eventDestination = a} :: CreateConfigurationSetEventDestination)

instance
  Core.AWSRequest
    CreateConfigurationSetEventDestination
  where
  type
    AWSResponse
      CreateConfigurationSetEventDestination =
      CreateConfigurationSetEventDestinationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetEventDestinationResult"
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateConfigurationSetEventDestination

instance
  Core.NFData
    CreateConfigurationSetEventDestination

instance
  Core.ToHeaders
    CreateConfigurationSetEventDestination
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    CreateConfigurationSetEventDestination
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateConfigurationSetEventDestination
  where
  toQuery CreateConfigurationSetEventDestination' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "CreateConfigurationSetEventDestination" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "ConfigurationSetName" Core.=: configurationSetName,
        "EventDestination" Core.=: eventDestination
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateConfigurationSetEventDestinationResponse' smart constructor.
data CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConfigurationSetEventDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfigurationSetEventDestinationResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetEventDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateConfigurationSetEventDestinationResponse
newCreateConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    CreateConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' CreateConfigurationSetEventDestinationResponse Core.Int
createConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\CreateConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetEventDestinationResponse)

instance
  Core.NFData
    CreateConfigurationSetEventDestinationResponse
