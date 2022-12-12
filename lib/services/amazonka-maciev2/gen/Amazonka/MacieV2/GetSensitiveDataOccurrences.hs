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
-- Module      : Amazonka.MacieV2.GetSensitiveDataOccurrences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves occurrences of sensitive data reported by a finding.
module Amazonka.MacieV2.GetSensitiveDataOccurrences
  ( -- * Creating a Request
    GetSensitiveDataOccurrences (..),
    newGetSensitiveDataOccurrences,

    -- * Request Lenses
    getSensitiveDataOccurrences_findingId,

    -- * Destructuring the Response
    GetSensitiveDataOccurrencesResponse (..),
    newGetSensitiveDataOccurrencesResponse,

    -- * Response Lenses
    getSensitiveDataOccurrencesResponse_error,
    getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences,
    getSensitiveDataOccurrencesResponse_status,
    getSensitiveDataOccurrencesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSensitiveDataOccurrences' smart constructor.
data GetSensitiveDataOccurrences = GetSensitiveDataOccurrences'
  { -- | The unique identifier for the finding.
    findingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitiveDataOccurrences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingId', 'getSensitiveDataOccurrences_findingId' - The unique identifier for the finding.
newGetSensitiveDataOccurrences ::
  -- | 'findingId'
  Prelude.Text ->
  GetSensitiveDataOccurrences
newGetSensitiveDataOccurrences pFindingId_ =
  GetSensitiveDataOccurrences'
    { findingId =
        pFindingId_
    }

-- | The unique identifier for the finding.
getSensitiveDataOccurrences_findingId :: Lens.Lens' GetSensitiveDataOccurrences Prelude.Text
getSensitiveDataOccurrences_findingId = Lens.lens (\GetSensitiveDataOccurrences' {findingId} -> findingId) (\s@GetSensitiveDataOccurrences' {} a -> s {findingId = a} :: GetSensitiveDataOccurrences)

instance Core.AWSRequest GetSensitiveDataOccurrences where
  type
    AWSResponse GetSensitiveDataOccurrences =
      GetSensitiveDataOccurrencesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSensitiveDataOccurrencesResponse'
            Prelude.<$> (x Data..?> "error")
            Prelude.<*> ( x Data..?> "sensitiveDataOccurrences"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSensitiveDataOccurrences where
  hashWithSalt _salt GetSensitiveDataOccurrences' {..} =
    _salt `Prelude.hashWithSalt` findingId

instance Prelude.NFData GetSensitiveDataOccurrences where
  rnf GetSensitiveDataOccurrences' {..} =
    Prelude.rnf findingId

instance Data.ToHeaders GetSensitiveDataOccurrences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSensitiveDataOccurrences where
  toPath GetSensitiveDataOccurrences' {..} =
    Prelude.mconcat
      ["/findings/", Data.toBS findingId, "/reveal"]

instance Data.ToQuery GetSensitiveDataOccurrences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSensitiveDataOccurrencesResponse' smart constructor.
data GetSensitiveDataOccurrencesResponse = GetSensitiveDataOccurrencesResponse'
  { -- | If an error occurred when Amazon Macie attempted to retrieve occurrences
    -- of sensitive data reported by the finding, a description of the error
    -- that occurred. This value is null if the status (status) of the request
    -- is PROCESSING or SUCCESS.
    error :: Prelude.Maybe Prelude.Text,
    -- | A map that specifies 1-100 types of sensitive data reported by the
    -- finding and, for each type, 1-10 occurrences of sensitive data.
    sensitiveDataOccurrences :: Prelude.Maybe (Prelude.HashMap Prelude.Text [DetectedDataDetails]),
    -- | The status of the request to retrieve occurrences of sensitive data
    -- reported by the finding. Possible values are:
    --
    -- -   ERROR - An error occurred when Amazon Macie attempted to locate,
    --     retrieve, or encrypt the sensitive data. The error value indicates
    --     the nature of the error that occurred.
    --
    -- -   PROCESSING - Macie is processing the request.
    --
    -- -   SUCCESS - Macie successfully located, retrieved, and encrypted the
    --     sensitive data.
    status :: Prelude.Maybe RevealRequestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitiveDataOccurrencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'getSensitiveDataOccurrencesResponse_error' - If an error occurred when Amazon Macie attempted to retrieve occurrences
-- of sensitive data reported by the finding, a description of the error
-- that occurred. This value is null if the status (status) of the request
-- is PROCESSING or SUCCESS.
--
-- 'sensitiveDataOccurrences', 'getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences' - A map that specifies 1-100 types of sensitive data reported by the
-- finding and, for each type, 1-10 occurrences of sensitive data.
--
-- 'status', 'getSensitiveDataOccurrencesResponse_status' - The status of the request to retrieve occurrences of sensitive data
-- reported by the finding. Possible values are:
--
-- -   ERROR - An error occurred when Amazon Macie attempted to locate,
--     retrieve, or encrypt the sensitive data. The error value indicates
--     the nature of the error that occurred.
--
-- -   PROCESSING - Macie is processing the request.
--
-- -   SUCCESS - Macie successfully located, retrieved, and encrypted the
--     sensitive data.
--
-- 'httpStatus', 'getSensitiveDataOccurrencesResponse_httpStatus' - The response's http status code.
newGetSensitiveDataOccurrencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSensitiveDataOccurrencesResponse
newGetSensitiveDataOccurrencesResponse pHttpStatus_ =
  GetSensitiveDataOccurrencesResponse'
    { error =
        Prelude.Nothing,
      sensitiveDataOccurrences =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If an error occurred when Amazon Macie attempted to retrieve occurrences
-- of sensitive data reported by the finding, a description of the error
-- that occurred. This value is null if the status (status) of the request
-- is PROCESSING or SUCCESS.
getSensitiveDataOccurrencesResponse_error :: Lens.Lens' GetSensitiveDataOccurrencesResponse (Prelude.Maybe Prelude.Text)
getSensitiveDataOccurrencesResponse_error = Lens.lens (\GetSensitiveDataOccurrencesResponse' {error} -> error) (\s@GetSensitiveDataOccurrencesResponse' {} a -> s {error = a} :: GetSensitiveDataOccurrencesResponse)

-- | A map that specifies 1-100 types of sensitive data reported by the
-- finding and, for each type, 1-10 occurrences of sensitive data.
getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences :: Lens.Lens' GetSensitiveDataOccurrencesResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text [DetectedDataDetails]))
getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences = Lens.lens (\GetSensitiveDataOccurrencesResponse' {sensitiveDataOccurrences} -> sensitiveDataOccurrences) (\s@GetSensitiveDataOccurrencesResponse' {} a -> s {sensitiveDataOccurrences = a} :: GetSensitiveDataOccurrencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the request to retrieve occurrences of sensitive data
-- reported by the finding. Possible values are:
--
-- -   ERROR - An error occurred when Amazon Macie attempted to locate,
--     retrieve, or encrypt the sensitive data. The error value indicates
--     the nature of the error that occurred.
--
-- -   PROCESSING - Macie is processing the request.
--
-- -   SUCCESS - Macie successfully located, retrieved, and encrypted the
--     sensitive data.
getSensitiveDataOccurrencesResponse_status :: Lens.Lens' GetSensitiveDataOccurrencesResponse (Prelude.Maybe RevealRequestStatus)
getSensitiveDataOccurrencesResponse_status = Lens.lens (\GetSensitiveDataOccurrencesResponse' {status} -> status) (\s@GetSensitiveDataOccurrencesResponse' {} a -> s {status = a} :: GetSensitiveDataOccurrencesResponse)

-- | The response's http status code.
getSensitiveDataOccurrencesResponse_httpStatus :: Lens.Lens' GetSensitiveDataOccurrencesResponse Prelude.Int
getSensitiveDataOccurrencesResponse_httpStatus = Lens.lens (\GetSensitiveDataOccurrencesResponse' {httpStatus} -> httpStatus) (\s@GetSensitiveDataOccurrencesResponse' {} a -> s {httpStatus = a} :: GetSensitiveDataOccurrencesResponse)

instance
  Prelude.NFData
    GetSensitiveDataOccurrencesResponse
  where
  rnf GetSensitiveDataOccurrencesResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf sensitiveDataOccurrences
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
