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
-- Module      : Amazonka.SES.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.SES.CreateConfigurationSetEventDestination
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

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
    configurationSetName :: Prelude.Text,
    -- | An object that describes the AWS service that email sending event
    -- information will be published to.
    eventDestination :: EventDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
createConfigurationSetEventDestination_configurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Prelude.Text
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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateConfigurationSetEventDestinationResult"
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConfigurationSetEventDestination
  where
  hashWithSalt
    _salt
    CreateConfigurationSetEventDestination' {..} =
      _salt
        `Prelude.hashWithSalt` configurationSetName
        `Prelude.hashWithSalt` eventDestination

instance
  Prelude.NFData
    CreateConfigurationSetEventDestination
  where
  rnf CreateConfigurationSetEventDestination' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf eventDestination

instance
  Data.ToHeaders
    CreateConfigurationSetEventDestination
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateConfigurationSetEventDestination
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateConfigurationSetEventDestination
  where
  toQuery CreateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateConfigurationSetEventDestination" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ConfigurationSetName" Data.=: configurationSetName,
        "EventDestination" Data.=: eventDestination
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newCreateConfigurationSetEventDestinationResponse' smart constructor.
data CreateConfigurationSetEventDestinationResponse = CreateConfigurationSetEventDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateConfigurationSetEventDestinationResponse
newCreateConfigurationSetEventDestinationResponse
  pHttpStatus_ =
    CreateConfigurationSetEventDestinationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
createConfigurationSetEventDestinationResponse_httpStatus :: Lens.Lens' CreateConfigurationSetEventDestinationResponse Prelude.Int
createConfigurationSetEventDestinationResponse_httpStatus = Lens.lens (\CreateConfigurationSetEventDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetEventDestinationResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetEventDestinationResponse)

instance
  Prelude.NFData
    CreateConfigurationSetEventDestinationResponse
  where
  rnf
    CreateConfigurationSetEventDestinationResponse' {..} =
      Prelude.rnf httpStatus
