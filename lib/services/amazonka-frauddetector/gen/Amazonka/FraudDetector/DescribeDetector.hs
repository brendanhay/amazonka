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
-- Module      : Amazonka.FraudDetector.DescribeDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all versions for a specified detector.
module Amazonka.FraudDetector.DescribeDetector
  ( -- * Creating a Request
    DescribeDetector (..),
    newDescribeDetector,

    -- * Request Lenses
    describeDetector_maxResults,
    describeDetector_nextToken,
    describeDetector_detectorId,

    -- * Destructuring the Response
    DescribeDetectorResponse (..),
    newDescribeDetectorResponse,

    -- * Response Lenses
    describeDetectorResponse_arn,
    describeDetectorResponse_detectorId,
    describeDetectorResponse_detectorVersionSummaries,
    describeDetectorResponse_nextToken,
    describeDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDetector' smart constructor.
data DescribeDetector = DescribeDetector'
  { -- | The maximum number of results to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next token from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeDetector_maxResults' - The maximum number of results to return for the request.
--
-- 'nextToken', 'describeDetector_nextToken' - The next token from the previous response.
--
-- 'detectorId', 'describeDetector_detectorId' - The detector ID.
newDescribeDetector ::
  -- | 'detectorId'
  Prelude.Text ->
  DescribeDetector
newDescribeDetector pDetectorId_ =
  DescribeDetector'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | The maximum number of results to return for the request.
describeDetector_maxResults :: Lens.Lens' DescribeDetector (Prelude.Maybe Prelude.Natural)
describeDetector_maxResults = Lens.lens (\DescribeDetector' {maxResults} -> maxResults) (\s@DescribeDetector' {} a -> s {maxResults = a} :: DescribeDetector)

-- | The next token from the previous response.
describeDetector_nextToken :: Lens.Lens' DescribeDetector (Prelude.Maybe Prelude.Text)
describeDetector_nextToken = Lens.lens (\DescribeDetector' {nextToken} -> nextToken) (\s@DescribeDetector' {} a -> s {nextToken = a} :: DescribeDetector)

-- | The detector ID.
describeDetector_detectorId :: Lens.Lens' DescribeDetector Prelude.Text
describeDetector_detectorId = Lens.lens (\DescribeDetector' {detectorId} -> detectorId) (\s@DescribeDetector' {} a -> s {detectorId = a} :: DescribeDetector)

instance Core.AWSRequest DescribeDetector where
  type
    AWSResponse DescribeDetector =
      DescribeDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDetectorResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "detectorId")
            Prelude.<*> ( x
                            Data..?> "detectorVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDetector where
  hashWithSalt _salt DescribeDetector' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` detectorId

instance Prelude.NFData DescribeDetector where
  rnf DescribeDetector' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf detectorId

instance Data.ToHeaders DescribeDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DescribeDetector" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDetector where
  toJSON DescribeDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("detectorId" Data..= detectorId)
          ]
      )

instance Data.ToPath DescribeDetector where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDetectorResponse' smart constructor.
data DescribeDetectorResponse = DescribeDetectorResponse'
  { -- | The detector ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The status and description for each detector version.
    detectorVersionSummaries :: Prelude.Maybe [DetectorVersionSummary],
    -- | The next token to be used for subsequent requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeDetectorResponse_arn' - The detector ARN.
--
-- 'detectorId', 'describeDetectorResponse_detectorId' - The detector ID.
--
-- 'detectorVersionSummaries', 'describeDetectorResponse_detectorVersionSummaries' - The status and description for each detector version.
--
-- 'nextToken', 'describeDetectorResponse_nextToken' - The next token to be used for subsequent requests.
--
-- 'httpStatus', 'describeDetectorResponse_httpStatus' - The response's http status code.
newDescribeDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDetectorResponse
newDescribeDetectorResponse pHttpStatus_ =
  DescribeDetectorResponse'
    { arn = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      detectorVersionSummaries = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detector ARN.
describeDetectorResponse_arn :: Lens.Lens' DescribeDetectorResponse (Prelude.Maybe Prelude.Text)
describeDetectorResponse_arn = Lens.lens (\DescribeDetectorResponse' {arn} -> arn) (\s@DescribeDetectorResponse' {} a -> s {arn = a} :: DescribeDetectorResponse)

-- | The detector ID.
describeDetectorResponse_detectorId :: Lens.Lens' DescribeDetectorResponse (Prelude.Maybe Prelude.Text)
describeDetectorResponse_detectorId = Lens.lens (\DescribeDetectorResponse' {detectorId} -> detectorId) (\s@DescribeDetectorResponse' {} a -> s {detectorId = a} :: DescribeDetectorResponse)

-- | The status and description for each detector version.
describeDetectorResponse_detectorVersionSummaries :: Lens.Lens' DescribeDetectorResponse (Prelude.Maybe [DetectorVersionSummary])
describeDetectorResponse_detectorVersionSummaries = Lens.lens (\DescribeDetectorResponse' {detectorVersionSummaries} -> detectorVersionSummaries) (\s@DescribeDetectorResponse' {} a -> s {detectorVersionSummaries = a} :: DescribeDetectorResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token to be used for subsequent requests.
describeDetectorResponse_nextToken :: Lens.Lens' DescribeDetectorResponse (Prelude.Maybe Prelude.Text)
describeDetectorResponse_nextToken = Lens.lens (\DescribeDetectorResponse' {nextToken} -> nextToken) (\s@DescribeDetectorResponse' {} a -> s {nextToken = a} :: DescribeDetectorResponse)

-- | The response's http status code.
describeDetectorResponse_httpStatus :: Lens.Lens' DescribeDetectorResponse Prelude.Int
describeDetectorResponse_httpStatus = Lens.lens (\DescribeDetectorResponse' {httpStatus} -> httpStatus) (\s@DescribeDetectorResponse' {} a -> s {httpStatus = a} :: DescribeDetectorResponse)

instance Prelude.NFData DescribeDetectorResponse where
  rnf DescribeDetectorResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf detectorVersionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
