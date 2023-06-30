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
-- Module      : Amazonka.MacieV2.GetSensitiveDataOccurrencesAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks whether occurrences of sensitive data can be retrieved for a
-- finding.
module Amazonka.MacieV2.GetSensitiveDataOccurrencesAvailability
  ( -- * Creating a Request
    GetSensitiveDataOccurrencesAvailability (..),
    newGetSensitiveDataOccurrencesAvailability,

    -- * Request Lenses
    getSensitiveDataOccurrencesAvailability_findingId,

    -- * Destructuring the Response
    GetSensitiveDataOccurrencesAvailabilityResponse (..),
    newGetSensitiveDataOccurrencesAvailabilityResponse,

    -- * Response Lenses
    getSensitiveDataOccurrencesAvailabilityResponse_code,
    getSensitiveDataOccurrencesAvailabilityResponse_reasons,
    getSensitiveDataOccurrencesAvailabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSensitiveDataOccurrencesAvailability' smart constructor.
data GetSensitiveDataOccurrencesAvailability = GetSensitiveDataOccurrencesAvailability'
  { -- | The unique identifier for the finding.
    findingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitiveDataOccurrencesAvailability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingId', 'getSensitiveDataOccurrencesAvailability_findingId' - The unique identifier for the finding.
newGetSensitiveDataOccurrencesAvailability ::
  -- | 'findingId'
  Prelude.Text ->
  GetSensitiveDataOccurrencesAvailability
newGetSensitiveDataOccurrencesAvailability
  pFindingId_ =
    GetSensitiveDataOccurrencesAvailability'
      { findingId =
          pFindingId_
      }

-- | The unique identifier for the finding.
getSensitiveDataOccurrencesAvailability_findingId :: Lens.Lens' GetSensitiveDataOccurrencesAvailability Prelude.Text
getSensitiveDataOccurrencesAvailability_findingId = Lens.lens (\GetSensitiveDataOccurrencesAvailability' {findingId} -> findingId) (\s@GetSensitiveDataOccurrencesAvailability' {} a -> s {findingId = a} :: GetSensitiveDataOccurrencesAvailability)

instance
  Core.AWSRequest
    GetSensitiveDataOccurrencesAvailability
  where
  type
    AWSResponse
      GetSensitiveDataOccurrencesAvailability =
      GetSensitiveDataOccurrencesAvailabilityResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSensitiveDataOccurrencesAvailabilityResponse'
            Prelude.<$> (x Data..?> "code")
            Prelude.<*> (x Data..?> "reasons" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSensitiveDataOccurrencesAvailability
  where
  hashWithSalt
    _salt
    GetSensitiveDataOccurrencesAvailability' {..} =
      _salt `Prelude.hashWithSalt` findingId

instance
  Prelude.NFData
    GetSensitiveDataOccurrencesAvailability
  where
  rnf GetSensitiveDataOccurrencesAvailability' {..} =
    Prelude.rnf findingId

instance
  Data.ToHeaders
    GetSensitiveDataOccurrencesAvailability
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
    GetSensitiveDataOccurrencesAvailability
  where
  toPath GetSensitiveDataOccurrencesAvailability' {..} =
    Prelude.mconcat
      [ "/findings/",
        Data.toBS findingId,
        "/reveal/availability"
      ]

instance
  Data.ToQuery
    GetSensitiveDataOccurrencesAvailability
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSensitiveDataOccurrencesAvailabilityResponse' smart constructor.
data GetSensitiveDataOccurrencesAvailabilityResponse = GetSensitiveDataOccurrencesAvailabilityResponse'
  { -- | Specifies whether occurrences of sensitive data can be retrieved for the
    -- finding. Possible values are: AVAILABLE, the sensitive data can be
    -- retrieved; and, UNAVAILABLE, the sensitive data can\'t be retrieved. If
    -- this value is UNAVAILABLE, the reasons array indicates why the data
    -- can\'t be retrieved.
    code :: Prelude.Maybe AvailabilityCode,
    -- | Specifies why occurrences of sensitive data can\'t be retrieved for the
    -- finding. Possible values are:
    --
    -- -   INVALID_CLASSIFICATION_RESULT - Amazon Macie can\'t verify the
    --     location of the sensitive data to retrieve. There isn\'t a
    --     corresponding sensitive data discovery result for the finding. Or
    --     the sensitive data discovery result specified by the
    --     ClassificationDetails.detailedResultsLocation field of the finding
    --     isn\'t available, is malformed or corrupted, or uses an unsupported
    --     storage format.
    --
    -- -   OBJECT_EXCEEDS_SIZE_QUOTA - The storage size of the affected S3
    --     object exceeds the size quota for retrieving occurrences of
    --     sensitive data.
    --
    -- -   OBJECT_UNAVAILABLE - The affected S3 object isn\'t available. The
    --     object might have been renamed, moved, or deleted. Or the object was
    --     changed after Macie created the finding.
    --
    -- -   UNSUPPORTED_FINDING_TYPE - The specified finding isn\'t a sensitive
    --     data finding.
    --
    -- -   UNSUPPORTED_OBJECT_TYPE - The affected S3 object uses a file or
    --     storage format that Macie doesn\'t support for retrieving
    --     occurrences of sensitive data.
    --
    -- This value is null if sensitive data can be retrieved for the finding.
    reasons :: Prelude.Maybe [UnavailabilityReasonCode],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitiveDataOccurrencesAvailabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'getSensitiveDataOccurrencesAvailabilityResponse_code' - Specifies whether occurrences of sensitive data can be retrieved for the
-- finding. Possible values are: AVAILABLE, the sensitive data can be
-- retrieved; and, UNAVAILABLE, the sensitive data can\'t be retrieved. If
-- this value is UNAVAILABLE, the reasons array indicates why the data
-- can\'t be retrieved.
--
-- 'reasons', 'getSensitiveDataOccurrencesAvailabilityResponse_reasons' - Specifies why occurrences of sensitive data can\'t be retrieved for the
-- finding. Possible values are:
--
-- -   INVALID_CLASSIFICATION_RESULT - Amazon Macie can\'t verify the
--     location of the sensitive data to retrieve. There isn\'t a
--     corresponding sensitive data discovery result for the finding. Or
--     the sensitive data discovery result specified by the
--     ClassificationDetails.detailedResultsLocation field of the finding
--     isn\'t available, is malformed or corrupted, or uses an unsupported
--     storage format.
--
-- -   OBJECT_EXCEEDS_SIZE_QUOTA - The storage size of the affected S3
--     object exceeds the size quota for retrieving occurrences of
--     sensitive data.
--
-- -   OBJECT_UNAVAILABLE - The affected S3 object isn\'t available. The
--     object might have been renamed, moved, or deleted. Or the object was
--     changed after Macie created the finding.
--
-- -   UNSUPPORTED_FINDING_TYPE - The specified finding isn\'t a sensitive
--     data finding.
--
-- -   UNSUPPORTED_OBJECT_TYPE - The affected S3 object uses a file or
--     storage format that Macie doesn\'t support for retrieving
--     occurrences of sensitive data.
--
-- This value is null if sensitive data can be retrieved for the finding.
--
-- 'httpStatus', 'getSensitiveDataOccurrencesAvailabilityResponse_httpStatus' - The response's http status code.
newGetSensitiveDataOccurrencesAvailabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSensitiveDataOccurrencesAvailabilityResponse
newGetSensitiveDataOccurrencesAvailabilityResponse
  pHttpStatus_ =
    GetSensitiveDataOccurrencesAvailabilityResponse'
      { code =
          Prelude.Nothing,
        reasons = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Specifies whether occurrences of sensitive data can be retrieved for the
-- finding. Possible values are: AVAILABLE, the sensitive data can be
-- retrieved; and, UNAVAILABLE, the sensitive data can\'t be retrieved. If
-- this value is UNAVAILABLE, the reasons array indicates why the data
-- can\'t be retrieved.
getSensitiveDataOccurrencesAvailabilityResponse_code :: Lens.Lens' GetSensitiveDataOccurrencesAvailabilityResponse (Prelude.Maybe AvailabilityCode)
getSensitiveDataOccurrencesAvailabilityResponse_code = Lens.lens (\GetSensitiveDataOccurrencesAvailabilityResponse' {code} -> code) (\s@GetSensitiveDataOccurrencesAvailabilityResponse' {} a -> s {code = a} :: GetSensitiveDataOccurrencesAvailabilityResponse)

-- | Specifies why occurrences of sensitive data can\'t be retrieved for the
-- finding. Possible values are:
--
-- -   INVALID_CLASSIFICATION_RESULT - Amazon Macie can\'t verify the
--     location of the sensitive data to retrieve. There isn\'t a
--     corresponding sensitive data discovery result for the finding. Or
--     the sensitive data discovery result specified by the
--     ClassificationDetails.detailedResultsLocation field of the finding
--     isn\'t available, is malformed or corrupted, or uses an unsupported
--     storage format.
--
-- -   OBJECT_EXCEEDS_SIZE_QUOTA - The storage size of the affected S3
--     object exceeds the size quota for retrieving occurrences of
--     sensitive data.
--
-- -   OBJECT_UNAVAILABLE - The affected S3 object isn\'t available. The
--     object might have been renamed, moved, or deleted. Or the object was
--     changed after Macie created the finding.
--
-- -   UNSUPPORTED_FINDING_TYPE - The specified finding isn\'t a sensitive
--     data finding.
--
-- -   UNSUPPORTED_OBJECT_TYPE - The affected S3 object uses a file or
--     storage format that Macie doesn\'t support for retrieving
--     occurrences of sensitive data.
--
-- This value is null if sensitive data can be retrieved for the finding.
getSensitiveDataOccurrencesAvailabilityResponse_reasons :: Lens.Lens' GetSensitiveDataOccurrencesAvailabilityResponse (Prelude.Maybe [UnavailabilityReasonCode])
getSensitiveDataOccurrencesAvailabilityResponse_reasons = Lens.lens (\GetSensitiveDataOccurrencesAvailabilityResponse' {reasons} -> reasons) (\s@GetSensitiveDataOccurrencesAvailabilityResponse' {} a -> s {reasons = a} :: GetSensitiveDataOccurrencesAvailabilityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSensitiveDataOccurrencesAvailabilityResponse_httpStatus :: Lens.Lens' GetSensitiveDataOccurrencesAvailabilityResponse Prelude.Int
getSensitiveDataOccurrencesAvailabilityResponse_httpStatus = Lens.lens (\GetSensitiveDataOccurrencesAvailabilityResponse' {httpStatus} -> httpStatus) (\s@GetSensitiveDataOccurrencesAvailabilityResponse' {} a -> s {httpStatus = a} :: GetSensitiveDataOccurrencesAvailabilityResponse)

instance
  Prelude.NFData
    GetSensitiveDataOccurrencesAvailabilityResponse
  where
  rnf
    GetSensitiveDataOccurrencesAvailabilityResponse' {..} =
      Prelude.rnf code
        `Prelude.seq` Prelude.rnf reasons
        `Prelude.seq` Prelude.rnf httpStatus
