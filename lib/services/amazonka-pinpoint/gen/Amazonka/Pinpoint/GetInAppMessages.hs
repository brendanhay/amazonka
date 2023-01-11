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
-- Module      : Amazonka.Pinpoint.GetInAppMessages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the in-app messages targeted for the provided endpoint ID.
module Amazonka.Pinpoint.GetInAppMessages
  ( -- * Creating a Request
    GetInAppMessages (..),
    newGetInAppMessages,

    -- * Request Lenses
    getInAppMessages_applicationId,
    getInAppMessages_endpointId,

    -- * Destructuring the Response
    GetInAppMessagesResponse (..),
    newGetInAppMessagesResponse,

    -- * Response Lenses
    getInAppMessagesResponse_httpStatus,
    getInAppMessagesResponse_inAppMessagesResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInAppMessages' smart constructor.
data GetInAppMessages = GetInAppMessages'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the endpoint.
    endpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInAppMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getInAppMessages_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointId', 'getInAppMessages_endpointId' - The unique identifier for the endpoint.
newGetInAppMessages ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  GetInAppMessages
newGetInAppMessages pApplicationId_ pEndpointId_ =
  GetInAppMessages'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getInAppMessages_applicationId :: Lens.Lens' GetInAppMessages Prelude.Text
getInAppMessages_applicationId = Lens.lens (\GetInAppMessages' {applicationId} -> applicationId) (\s@GetInAppMessages' {} a -> s {applicationId = a} :: GetInAppMessages)

-- | The unique identifier for the endpoint.
getInAppMessages_endpointId :: Lens.Lens' GetInAppMessages Prelude.Text
getInAppMessages_endpointId = Lens.lens (\GetInAppMessages' {endpointId} -> endpointId) (\s@GetInAppMessages' {} a -> s {endpointId = a} :: GetInAppMessages)

instance Core.AWSRequest GetInAppMessages where
  type
    AWSResponse GetInAppMessages =
      GetInAppMessagesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInAppMessagesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetInAppMessages where
  hashWithSalt _salt GetInAppMessages' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` endpointId

instance Prelude.NFData GetInAppMessages where
  rnf GetInAppMessages' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf endpointId

instance Data.ToHeaders GetInAppMessages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetInAppMessages where
  toPath GetInAppMessages' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/endpoints/",
        Data.toBS endpointId,
        "/inappmessages"
      ]

instance Data.ToQuery GetInAppMessages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInAppMessagesResponse' smart constructor.
data GetInAppMessagesResponse = GetInAppMessagesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    inAppMessagesResponse :: InAppMessagesResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInAppMessagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getInAppMessagesResponse_httpStatus' - The response's http status code.
--
-- 'inAppMessagesResponse', 'getInAppMessagesResponse_inAppMessagesResponse' - Undocumented member.
newGetInAppMessagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'inAppMessagesResponse'
  InAppMessagesResponse ->
  GetInAppMessagesResponse
newGetInAppMessagesResponse
  pHttpStatus_
  pInAppMessagesResponse_ =
    GetInAppMessagesResponse'
      { httpStatus =
          pHttpStatus_,
        inAppMessagesResponse = pInAppMessagesResponse_
      }

-- | The response's http status code.
getInAppMessagesResponse_httpStatus :: Lens.Lens' GetInAppMessagesResponse Prelude.Int
getInAppMessagesResponse_httpStatus = Lens.lens (\GetInAppMessagesResponse' {httpStatus} -> httpStatus) (\s@GetInAppMessagesResponse' {} a -> s {httpStatus = a} :: GetInAppMessagesResponse)

-- | Undocumented member.
getInAppMessagesResponse_inAppMessagesResponse :: Lens.Lens' GetInAppMessagesResponse InAppMessagesResponse
getInAppMessagesResponse_inAppMessagesResponse = Lens.lens (\GetInAppMessagesResponse' {inAppMessagesResponse} -> inAppMessagesResponse) (\s@GetInAppMessagesResponse' {} a -> s {inAppMessagesResponse = a} :: GetInAppMessagesResponse)

instance Prelude.NFData GetInAppMessagesResponse where
  rnf GetInAppMessagesResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf inAppMessagesResponse
