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
-- Module      : Amazonka.SmsVoice.GetConfigurationSetEventDestinations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtain information about an event destination, including the types of
-- events it reports, the Amazon Resource Name (ARN) of the destination,
-- and the name of the event destination.
module Amazonka.SmsVoice.GetConfigurationSetEventDestinations
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SmsVoice.Types

-- | /See:/ 'newGetConfigurationSetEventDestinations' smart constructor.
data GetConfigurationSetEventDestinations = GetConfigurationSetEventDestinations'
  { -- | ConfigurationSetName
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
-- 'configurationSetName', 'getConfigurationSetEventDestinations_configurationSetName' - ConfigurationSetName
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

-- | ConfigurationSetName
getConfigurationSetEventDestinations_configurationSetName :: Lens.Lens' GetConfigurationSetEventDestinations Prelude.Text
getConfigurationSetEventDestinations_configurationSetName = Lens.lens (\GetConfigurationSetEventDestinations' {configurationSetName} -> configurationSetName) (\s@GetConfigurationSetEventDestinations' {} a -> s {configurationSetName = a} :: GetConfigurationSetEventDestinations)

instance
  Core.AWSRequest
    GetConfigurationSetEventDestinations
  where
  type
    AWSResponse GetConfigurationSetEventDestinations =
      GetConfigurationSetEventDestinationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigurationSetEventDestinationsResponse'
            Prelude.<$> ( x
                            Data..?> "EventDestinations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetConfigurationSetEventDestinations
  where
  hashWithSalt
    _salt
    GetConfigurationSetEventDestinations' {..} =
      _salt `Prelude.hashWithSalt` configurationSetName

instance
  Prelude.NFData
    GetConfigurationSetEventDestinations
  where
  rnf GetConfigurationSetEventDestinations' {..} =
    Prelude.rnf configurationSetName

instance
  Data.ToHeaders
    GetConfigurationSetEventDestinations
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
  Data.ToPath
    GetConfigurationSetEventDestinations
  where
  toPath GetConfigurationSetEventDestinations' {..} =
    Prelude.mconcat
      [ "/v1/sms-voice/configuration-sets/",
        Data.toBS configurationSetName,
        "/event-destinations"
      ]

instance
  Data.ToQuery
    GetConfigurationSetEventDestinations
  where
  toQuery = Prelude.const Prelude.mempty

-- | An object that contains information about an event destination.
--
-- /See:/ 'newGetConfigurationSetEventDestinationsResponse' smart constructor.
data GetConfigurationSetEventDestinationsResponse = GetConfigurationSetEventDestinationsResponse'
  { eventDestinations :: Prelude.Maybe [EventDestination],
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
-- 'eventDestinations', 'getConfigurationSetEventDestinationsResponse_eventDestinations' - Undocumented member.
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

-- | Undocumented member.
getConfigurationSetEventDestinationsResponse_eventDestinations :: Lens.Lens' GetConfigurationSetEventDestinationsResponse (Prelude.Maybe [EventDestination])
getConfigurationSetEventDestinationsResponse_eventDestinations = Lens.lens (\GetConfigurationSetEventDestinationsResponse' {eventDestinations} -> eventDestinations) (\s@GetConfigurationSetEventDestinationsResponse' {} a -> s {eventDestinations = a} :: GetConfigurationSetEventDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getConfigurationSetEventDestinationsResponse_httpStatus :: Lens.Lens' GetConfigurationSetEventDestinationsResponse Prelude.Int
getConfigurationSetEventDestinationsResponse_httpStatus = Lens.lens (\GetConfigurationSetEventDestinationsResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationSetEventDestinationsResponse' {} a -> s {httpStatus = a} :: GetConfigurationSetEventDestinationsResponse)

instance
  Prelude.NFData
    GetConfigurationSetEventDestinationsResponse
  where
  rnf GetConfigurationSetEventDestinationsResponse' {..} =
    Prelude.rnf eventDestinations
      `Prelude.seq` Prelude.rnf httpStatus
