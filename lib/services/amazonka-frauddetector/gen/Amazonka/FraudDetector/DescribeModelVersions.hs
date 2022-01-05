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
-- Module      : Amazonka.FraudDetector.DescribeModelVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all of the model versions for the specified model type or for the
-- specified model type and model ID. You can also get details for a
-- single, specified model version.
module Amazonka.FraudDetector.DescribeModelVersions
  ( -- * Creating a Request
    DescribeModelVersions (..),
    newDescribeModelVersions,

    -- * Request Lenses
    describeModelVersions_modelType,
    describeModelVersions_modelId,
    describeModelVersions_nextToken,
    describeModelVersions_modelVersionNumber,
    describeModelVersions_maxResults,

    -- * Destructuring the Response
    DescribeModelVersionsResponse (..),
    newDescribeModelVersionsResponse,

    -- * Response Lenses
    describeModelVersionsResponse_nextToken,
    describeModelVersionsResponse_modelVersionDetails,
    describeModelVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeModelVersions' smart constructor.
data DescribeModelVersions = DescribeModelVersions'
  { -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The next token from the previous results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelType', 'describeModelVersions_modelType' - The model type.
--
-- 'modelId', 'describeModelVersions_modelId' - The model ID.
--
-- 'nextToken', 'describeModelVersions_nextToken' - The next token from the previous results.
--
-- 'modelVersionNumber', 'describeModelVersions_modelVersionNumber' - The model version number.
--
-- 'maxResults', 'describeModelVersions_maxResults' - The maximum number of results to return.
newDescribeModelVersions ::
  DescribeModelVersions
newDescribeModelVersions =
  DescribeModelVersions'
    { modelType = Prelude.Nothing,
      modelId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The model type.
describeModelVersions_modelType :: Lens.Lens' DescribeModelVersions (Prelude.Maybe ModelTypeEnum)
describeModelVersions_modelType = Lens.lens (\DescribeModelVersions' {modelType} -> modelType) (\s@DescribeModelVersions' {} a -> s {modelType = a} :: DescribeModelVersions)

-- | The model ID.
describeModelVersions_modelId :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_modelId = Lens.lens (\DescribeModelVersions' {modelId} -> modelId) (\s@DescribeModelVersions' {} a -> s {modelId = a} :: DescribeModelVersions)

-- | The next token from the previous results.
describeModelVersions_nextToken :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_nextToken = Lens.lens (\DescribeModelVersions' {nextToken} -> nextToken) (\s@DescribeModelVersions' {} a -> s {nextToken = a} :: DescribeModelVersions)

-- | The model version number.
describeModelVersions_modelVersionNumber :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_modelVersionNumber = Lens.lens (\DescribeModelVersions' {modelVersionNumber} -> modelVersionNumber) (\s@DescribeModelVersions' {} a -> s {modelVersionNumber = a} :: DescribeModelVersions)

-- | The maximum number of results to return.
describeModelVersions_maxResults :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Natural)
describeModelVersions_maxResults = Lens.lens (\DescribeModelVersions' {maxResults} -> maxResults) (\s@DescribeModelVersions' {} a -> s {maxResults = a} :: DescribeModelVersions)

instance Core.AWSRequest DescribeModelVersions where
  type
    AWSResponse DescribeModelVersions =
      DescribeModelVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "modelVersionDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeModelVersions where
  hashWithSalt _salt DescribeModelVersions' {..} =
    _salt `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeModelVersions where
  rnf DescribeModelVersions' {..} =
    Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeModelVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.DescribeModelVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeModelVersions where
  toJSON DescribeModelVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("modelType" Core..=) Prelude.<$> modelType,
            ("modelId" Core..=) Prelude.<$> modelId,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("modelVersionNumber" Core..=)
              Prelude.<$> modelVersionNumber,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeModelVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeModelVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelVersionsResponse' smart constructor.
data DescribeModelVersionsResponse = DescribeModelVersionsResponse'
  { -- | The next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The model version details.
    modelVersionDetails :: Prelude.Maybe [ModelVersionDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeModelVersionsResponse_nextToken' - The next token.
--
-- 'modelVersionDetails', 'describeModelVersionsResponse_modelVersionDetails' - The model version details.
--
-- 'httpStatus', 'describeModelVersionsResponse_httpStatus' - The response's http status code.
newDescribeModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeModelVersionsResponse
newDescribeModelVersionsResponse pHttpStatus_ =
  DescribeModelVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      modelVersionDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next token.
describeModelVersionsResponse_nextToken :: Lens.Lens' DescribeModelVersionsResponse (Prelude.Maybe Prelude.Text)
describeModelVersionsResponse_nextToken = Lens.lens (\DescribeModelVersionsResponse' {nextToken} -> nextToken) (\s@DescribeModelVersionsResponse' {} a -> s {nextToken = a} :: DescribeModelVersionsResponse)

-- | The model version details.
describeModelVersionsResponse_modelVersionDetails :: Lens.Lens' DescribeModelVersionsResponse (Prelude.Maybe [ModelVersionDetail])
describeModelVersionsResponse_modelVersionDetails = Lens.lens (\DescribeModelVersionsResponse' {modelVersionDetails} -> modelVersionDetails) (\s@DescribeModelVersionsResponse' {} a -> s {modelVersionDetails = a} :: DescribeModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeModelVersionsResponse_httpStatus :: Lens.Lens' DescribeModelVersionsResponse Prelude.Int
describeModelVersionsResponse_httpStatus = Lens.lens (\DescribeModelVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeModelVersionsResponse' {} a -> s {httpStatus = a} :: DescribeModelVersionsResponse)

instance Prelude.NFData DescribeModelVersionsResponse where
  rnf DescribeModelVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf modelVersionDetails
      `Prelude.seq` Prelude.rnf httpStatus
