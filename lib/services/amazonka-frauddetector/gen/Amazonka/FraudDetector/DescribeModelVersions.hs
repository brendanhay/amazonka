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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeModelVersions_maxResults,
    describeModelVersions_modelId,
    describeModelVersions_modelType,
    describeModelVersions_modelVersionNumber,
    describeModelVersions_nextToken,

    -- * Destructuring the Response
    DescribeModelVersionsResponse (..),
    newDescribeModelVersionsResponse,

    -- * Response Lenses
    describeModelVersionsResponse_modelVersionDetails,
    describeModelVersionsResponse_nextToken,
    describeModelVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeModelVersions' smart constructor.
data DescribeModelVersions = DescribeModelVersions'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The model ID.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The model type.
    modelType :: Prelude.Maybe ModelTypeEnum,
    -- | The model version number.
    modelVersionNumber :: Prelude.Maybe Prelude.Text,
    -- | The next token from the previous results.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'describeModelVersions_maxResults' - The maximum number of results to return.
--
-- 'modelId', 'describeModelVersions_modelId' - The model ID.
--
-- 'modelType', 'describeModelVersions_modelType' - The model type.
--
-- 'modelVersionNumber', 'describeModelVersions_modelVersionNumber' - The model version number.
--
-- 'nextToken', 'describeModelVersions_nextToken' - The next token from the previous results.
newDescribeModelVersions ::
  DescribeModelVersions
newDescribeModelVersions =
  DescribeModelVersions'
    { maxResults =
        Prelude.Nothing,
      modelId = Prelude.Nothing,
      modelType = Prelude.Nothing,
      modelVersionNumber = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
describeModelVersions_maxResults :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Natural)
describeModelVersions_maxResults = Lens.lens (\DescribeModelVersions' {maxResults} -> maxResults) (\s@DescribeModelVersions' {} a -> s {maxResults = a} :: DescribeModelVersions)

-- | The model ID.
describeModelVersions_modelId :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_modelId = Lens.lens (\DescribeModelVersions' {modelId} -> modelId) (\s@DescribeModelVersions' {} a -> s {modelId = a} :: DescribeModelVersions)

-- | The model type.
describeModelVersions_modelType :: Lens.Lens' DescribeModelVersions (Prelude.Maybe ModelTypeEnum)
describeModelVersions_modelType = Lens.lens (\DescribeModelVersions' {modelType} -> modelType) (\s@DescribeModelVersions' {} a -> s {modelType = a} :: DescribeModelVersions)

-- | The model version number.
describeModelVersions_modelVersionNumber :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_modelVersionNumber = Lens.lens (\DescribeModelVersions' {modelVersionNumber} -> modelVersionNumber) (\s@DescribeModelVersions' {} a -> s {modelVersionNumber = a} :: DescribeModelVersions)

-- | The next token from the previous results.
describeModelVersions_nextToken :: Lens.Lens' DescribeModelVersions (Prelude.Maybe Prelude.Text)
describeModelVersions_nextToken = Lens.lens (\DescribeModelVersions' {nextToken} -> nextToken) (\s@DescribeModelVersions' {} a -> s {nextToken = a} :: DescribeModelVersions)

instance Core.AWSRequest DescribeModelVersions where
  type
    AWSResponse DescribeModelVersions =
      DescribeModelVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelVersionsResponse'
            Prelude.<$> ( x
                            Data..?> "modelVersionDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeModelVersions where
  hashWithSalt _salt DescribeModelVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` modelType
      `Prelude.hashWithSalt` modelVersionNumber
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeModelVersions where
  rnf DescribeModelVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelId
      `Prelude.seq` Prelude.rnf modelType
      `Prelude.seq` Prelude.rnf modelVersionNumber
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeModelVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DescribeModelVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeModelVersions where
  toJSON DescribeModelVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("modelId" Data..=) Prelude.<$> modelId,
            ("modelType" Data..=) Prelude.<$> modelType,
            ("modelVersionNumber" Data..=)
              Prelude.<$> modelVersionNumber,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeModelVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeModelVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelVersionsResponse' smart constructor.
data DescribeModelVersionsResponse = DescribeModelVersionsResponse'
  { -- | The model version details.
    modelVersionDetails :: Prelude.Maybe [ModelVersionDetail],
    -- | The next token.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'modelVersionDetails', 'describeModelVersionsResponse_modelVersionDetails' - The model version details.
--
-- 'nextToken', 'describeModelVersionsResponse_nextToken' - The next token.
--
-- 'httpStatus', 'describeModelVersionsResponse_httpStatus' - The response's http status code.
newDescribeModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeModelVersionsResponse
newDescribeModelVersionsResponse pHttpStatus_ =
  DescribeModelVersionsResponse'
    { modelVersionDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The model version details.
describeModelVersionsResponse_modelVersionDetails :: Lens.Lens' DescribeModelVersionsResponse (Prelude.Maybe [ModelVersionDetail])
describeModelVersionsResponse_modelVersionDetails = Lens.lens (\DescribeModelVersionsResponse' {modelVersionDetails} -> modelVersionDetails) (\s@DescribeModelVersionsResponse' {} a -> s {modelVersionDetails = a} :: DescribeModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token.
describeModelVersionsResponse_nextToken :: Lens.Lens' DescribeModelVersionsResponse (Prelude.Maybe Prelude.Text)
describeModelVersionsResponse_nextToken = Lens.lens (\DescribeModelVersionsResponse' {nextToken} -> nextToken) (\s@DescribeModelVersionsResponse' {} a -> s {nextToken = a} :: DescribeModelVersionsResponse)

-- | The response's http status code.
describeModelVersionsResponse_httpStatus :: Lens.Lens' DescribeModelVersionsResponse Prelude.Int
describeModelVersionsResponse_httpStatus = Lens.lens (\DescribeModelVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeModelVersionsResponse' {} a -> s {httpStatus = a} :: DescribeModelVersionsResponse)

instance Prelude.NFData DescribeModelVersionsResponse where
  rnf DescribeModelVersionsResponse' {..} =
    Prelude.rnf modelVersionDetails
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
