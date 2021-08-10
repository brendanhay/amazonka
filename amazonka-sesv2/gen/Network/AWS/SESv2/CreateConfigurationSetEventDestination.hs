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
-- Module      : Network.AWS.SESv2.CreateConfigurationSetEventDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an event destination. /Events/ include message sends, deliveries,
-- opens, clicks, bounces, and complaints. /Event destinations/ are places
-- that you can send information about these events to. For example, you
-- can send event data to Amazon SNS to receive notifications when you
-- receive bounces or complaints, or you can use Amazon Kinesis Data
-- Firehose to stream data to Amazon S3 for long-term storage.
--
-- A single configuration set can include more than one event destination.
module Network.AWS.SESv2.CreateConfigurationSetEventDestination
  ( -- * Creating a Request
    CreateConfigurationSetEventDestination (..),
    newCreateConfigurationSetEventDestination,

    -- * Request Lenses
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestination_eventDestinationName,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to add an event destination to a configuration set.
--
-- /See:/ 'newCreateConfigurationSetEventDestination' smart constructor.
data CreateConfigurationSetEventDestination = CreateConfigurationSetEventDestination'
  { -- | The name of the configuration set that you want to add an event
    -- destination to.
    configurationSetName :: Prelude.Text,
    -- | A name that identifies the event destination within the configuration
    -- set.
    eventDestinationName :: Prelude.Text,
    -- | An object that defines the event destination.
    eventDestination :: EventDestinationDefinition
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
-- 'configurationSetName', 'createConfigurationSetEventDestination_configurationSetName' - The name of the configuration set that you want to add an event
-- destination to.
--
-- 'eventDestinationName', 'createConfigurationSetEventDestination_eventDestinationName' - A name that identifies the event destination within the configuration
-- set.
--
-- 'eventDestination', 'createConfigurationSetEventDestination_eventDestination' - An object that defines the event destination.
newCreateConfigurationSetEventDestination ::
  -- | 'configurationSetName'
  Prelude.Text ->
  -- | 'eventDestinationName'
  Prelude.Text ->
  -- | 'eventDestination'
  EventDestinationDefinition ->
  CreateConfigurationSetEventDestination
newCreateConfigurationSetEventDestination
  pConfigurationSetName_
  pEventDestinationName_
  pEventDestination_ =
    CreateConfigurationSetEventDestination'
      { configurationSetName =
          pConfigurationSetName_,
        eventDestinationName =
          pEventDestinationName_,
        eventDestination =
          pEventDestination_
      }

-- | The name of the configuration set that you want to add an event
-- destination to.
createConfigurationSetEventDestination_configurationSetName :: Lens.Lens' CreateConfigurationSetEventDestination Prelude.Text
createConfigurationSetEventDestination_configurationSetName = Lens.lens (\CreateConfigurationSetEventDestination' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSetEventDestination' {} a -> s {configurationSetName = a} :: CreateConfigurationSetEventDestination)

-- | A name that identifies the event destination within the configuration
-- set.
createConfigurationSetEventDestination_eventDestinationName :: Lens.Lens' CreateConfigurationSetEventDestination Prelude.Text
createConfigurationSetEventDestination_eventDestinationName = Lens.lens (\CreateConfigurationSetEventDestination' {eventDestinationName} -> eventDestinationName) (\s@CreateConfigurationSetEventDestination' {} a -> s {eventDestinationName = a} :: CreateConfigurationSetEventDestination)

-- | An object that defines the event destination.
createConfigurationSetEventDestination_eventDestination :: Lens.Lens' CreateConfigurationSetEventDestination EventDestinationDefinition
createConfigurationSetEventDestination_eventDestination = Lens.lens (\CreateConfigurationSetEventDestination' {eventDestination} -> eventDestination) (\s@CreateConfigurationSetEventDestination' {} a -> s {eventDestination = a} :: CreateConfigurationSetEventDestination)

instance
  Core.AWSRequest
    CreateConfigurationSetEventDestination
  where
  type
    AWSResponse
      CreateConfigurationSetEventDestination =
      CreateConfigurationSetEventDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConfigurationSetEventDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateConfigurationSetEventDestination

instance
  Prelude.NFData
    CreateConfigurationSetEventDestination

instance
  Core.ToHeaders
    CreateConfigurationSetEventDestination
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
  Core.ToJSON
    CreateConfigurationSetEventDestination
  where
  toJSON CreateConfigurationSetEventDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EventDestinationName"
                  Core..= eventDestinationName
              ),
            Prelude.Just
              ("EventDestination" Core..= eventDestination)
          ]
      )

instance
  Core.ToPath
    CreateConfigurationSetEventDestination
  where
  toPath CreateConfigurationSetEventDestination' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/event-destinations"
      ]

instance
  Core.ToQuery
    CreateConfigurationSetEventDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
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
