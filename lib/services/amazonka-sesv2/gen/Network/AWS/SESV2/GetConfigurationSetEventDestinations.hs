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
-- Module      : Network.AWS.SESV2.GetConfigurationSetEventDestinations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of event destinations that are associated with a
-- configuration set.
--
-- /Events/ include message sends, deliveries, opens, clicks, bounces, and
-- complaints. /Event destinations/ are places that you can send
-- information about these events to. For example, you can send event data
-- to Amazon SNS to receive notifications when you receive bounces or
-- complaints, or you can use Amazon Kinesis Data Firehose to stream data
-- to Amazon S3 for long-term storage.
module Network.AWS.SESV2.GetConfigurationSetEventDestinations
  ( -- * Creating a Request
    GetConfigurationSetEventDestinations (..),
    newGetConfigurationSetEventDestinations,

    -- * Request Lenses
    getConfigurationSetEventDestinations_configurationSetName,

    -- * Destructuring the Response
    GetConfigurationSetEventDestinationsResponse (..),
    newGetConfigurationSetEventDestinationsResponse,

    -- * Response Lenses
    getConfigurationSetEventDestinationsResponse_eventDestinations,
    getConfigurationSetEventDestinationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESV2.Types

-- | A request to obtain information about the event destinations for a
-- configuration set.
--
-- /See:/ 'newGetConfigurationSetEventDestinations' smart constructor.
data GetConfigurationSetEventDestinations = GetConfigurationSetEventDestinations'
  { -- | The name of the configuration set that contains the event destination.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationSetEventDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'getConfigurationSetEventDestinations_configurationSetName' - The name of the configuration set that contains the event destination.
newGetConfigurationSetEventDestinations ::
  -- | 'configurationSetName'
  Prelude.Text ->
  GetConfigurationSetEventDestinations
newGetConfigurationSetEventDestinations
  pConfigurationSetName_ =
    GetConfigurationSetEventDestinations'
      { configurationSetName =
          pConfigurationSetName_
      }

-- | The name of the configuration set that contains the event destination.
getConfigurationSetEventDestinations_configurationSetName :: Lens.Lens' GetConfigurationSetEventDestinations Prelude.Text
getConfigurationSetEventDestinations_configurationSetName = Lens.lens (\GetConfigurationSetEventDestinations' {configurationSetName} -> configurationSetName) (\s@GetConfigurationSetEventDestinations' {} a -> s {configurationSetName = a} :: GetConfigurationSetEventDestinations)

instance
  Core.AWSRequest
    GetConfigurationSetEventDestinations
  where
  type
    AWSResponse GetConfigurationSetEventDestinations =
      GetConfigurationSetEventDestinationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigurationSetEventDestinationsResponse'
            Prelude.<$> ( x Core..?> "EventDestinations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetConfigurationSetEventDestinations

instance
  Prelude.NFData
    GetConfigurationSetEventDestinations

instance
  Core.ToHeaders
    GetConfigurationSetEventDestinations
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
    GetConfigurationSetEventDestinations
  where
  toPath GetConfigurationSetEventDestinations' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Core.toBS configurationSetName,
        "/event-destinations"
      ]

instance
  Core.ToQuery
    GetConfigurationSetEventDestinations
  where
  toQuery = Prelude.const Prelude.mempty

-- | Information about an event destination for a configuration set.
--
-- /See:/ 'newGetConfigurationSetEventDestinationsResponse' smart constructor.
data GetConfigurationSetEventDestinationsResponse = GetConfigurationSetEventDestinationsResponse'
  { -- | An array that includes all of the events destinations that have been
    -- configured for the configuration set.
    eventDestinations :: Prelude.Maybe [EventDestination],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationSetEventDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventDestinations', 'getConfigurationSetEventDestinationsResponse_eventDestinations' - An array that includes all of the events destinations that have been
-- configured for the configuration set.
--
-- 'httpStatus', 'getConfigurationSetEventDestinationsResponse_httpStatus' - The response's http status code.
newGetConfigurationSetEventDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigurationSetEventDestinationsResponse
newGetConfigurationSetEventDestinationsResponse
  pHttpStatus_ =
    GetConfigurationSetEventDestinationsResponse'
      { eventDestinations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array that includes all of the events destinations that have been
-- configured for the configuration set.
getConfigurationSetEventDestinationsResponse_eventDestinations :: Lens.Lens' GetConfigurationSetEventDestinationsResponse (Prelude.Maybe [EventDestination])
getConfigurationSetEventDestinationsResponse_eventDestinations = Lens.lens (\GetConfigurationSetEventDestinationsResponse' {eventDestinations} -> eventDestinations) (\s@GetConfigurationSetEventDestinationsResponse' {} a -> s {eventDestinations = a} :: GetConfigurationSetEventDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getConfigurationSetEventDestinationsResponse_httpStatus :: Lens.Lens' GetConfigurationSetEventDestinationsResponse Prelude.Int
getConfigurationSetEventDestinationsResponse_httpStatus = Lens.lens (\GetConfigurationSetEventDestinationsResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationSetEventDestinationsResponse' {} a -> s {httpStatus = a} :: GetConfigurationSetEventDestinationsResponse)

instance
  Prelude.NFData
    GetConfigurationSetEventDestinationsResponse
