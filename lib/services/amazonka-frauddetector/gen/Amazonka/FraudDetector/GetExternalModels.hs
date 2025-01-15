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
-- Module      : Amazonka.FraudDetector.GetExternalModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details for one or more Amazon SageMaker models that have been
-- imported into the service. This is a paginated API. If you provide a
-- null @maxResults@, this actions retrieves a maximum of 10 records per
-- page. If you provide a @maxResults@, the value must be between 5 and 10.
-- To get the next page results, provide the pagination token from the
-- @GetExternalModelsResult@ as part of your request. A null pagination
-- token fetches the records from the beginning.
module Amazonka.FraudDetector.GetExternalModels
  ( -- * Creating a Request
    GetExternalModels (..),
    newGetExternalModels,

    -- * Request Lenses
    getExternalModels_maxResults,
    getExternalModels_modelEndpoint,
    getExternalModels_nextToken,

    -- * Destructuring the Response
    GetExternalModelsResponse (..),
    newGetExternalModelsResponse,

    -- * Response Lenses
    getExternalModelsResponse_externalModels,
    getExternalModelsResponse_nextToken,
    getExternalModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExternalModels' smart constructor.
data GetExternalModels = GetExternalModels'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon SageMaker model endpoint.
    modelEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The next page token for the request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExternalModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getExternalModels_maxResults' - The maximum number of objects to return for the request.
--
-- 'modelEndpoint', 'getExternalModels_modelEndpoint' - The Amazon SageMaker model endpoint.
--
-- 'nextToken', 'getExternalModels_nextToken' - The next page token for the request.
newGetExternalModels ::
  GetExternalModels
newGetExternalModels =
  GetExternalModels'
    { maxResults = Prelude.Nothing,
      modelEndpoint = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects to return for the request.
getExternalModels_maxResults :: Lens.Lens' GetExternalModels (Prelude.Maybe Prelude.Natural)
getExternalModels_maxResults = Lens.lens (\GetExternalModels' {maxResults} -> maxResults) (\s@GetExternalModels' {} a -> s {maxResults = a} :: GetExternalModels)

-- | The Amazon SageMaker model endpoint.
getExternalModels_modelEndpoint :: Lens.Lens' GetExternalModels (Prelude.Maybe Prelude.Text)
getExternalModels_modelEndpoint = Lens.lens (\GetExternalModels' {modelEndpoint} -> modelEndpoint) (\s@GetExternalModels' {} a -> s {modelEndpoint = a} :: GetExternalModels)

-- | The next page token for the request.
getExternalModels_nextToken :: Lens.Lens' GetExternalModels (Prelude.Maybe Prelude.Text)
getExternalModels_nextToken = Lens.lens (\GetExternalModels' {nextToken} -> nextToken) (\s@GetExternalModels' {} a -> s {nextToken = a} :: GetExternalModels)

instance Core.AWSRequest GetExternalModels where
  type
    AWSResponse GetExternalModels =
      GetExternalModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExternalModelsResponse'
            Prelude.<$> (x Data..?> "externalModels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExternalModels where
  hashWithSalt _salt GetExternalModels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelEndpoint
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetExternalModels where
  rnf GetExternalModels' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf modelEndpoint `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders GetExternalModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetExternalModels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetExternalModels where
  toJSON GetExternalModels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("modelEndpoint" Data..=) Prelude.<$> modelEndpoint,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetExternalModels where
  toPath = Prelude.const "/"

instance Data.ToQuery GetExternalModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExternalModelsResponse' smart constructor.
data GetExternalModelsResponse = GetExternalModelsResponse'
  { -- | Gets the Amazon SageMaker models.
    externalModels :: Prelude.Maybe [ExternalModel],
    -- | The next page token to be used in subsequent requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExternalModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalModels', 'getExternalModelsResponse_externalModels' - Gets the Amazon SageMaker models.
--
-- 'nextToken', 'getExternalModelsResponse_nextToken' - The next page token to be used in subsequent requests.
--
-- 'httpStatus', 'getExternalModelsResponse_httpStatus' - The response's http status code.
newGetExternalModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExternalModelsResponse
newGetExternalModelsResponse pHttpStatus_ =
  GetExternalModelsResponse'
    { externalModels =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Gets the Amazon SageMaker models.
getExternalModelsResponse_externalModels :: Lens.Lens' GetExternalModelsResponse (Prelude.Maybe [ExternalModel])
getExternalModelsResponse_externalModels = Lens.lens (\GetExternalModelsResponse' {externalModels} -> externalModels) (\s@GetExternalModelsResponse' {} a -> s {externalModels = a} :: GetExternalModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token to be used in subsequent requests.
getExternalModelsResponse_nextToken :: Lens.Lens' GetExternalModelsResponse (Prelude.Maybe Prelude.Text)
getExternalModelsResponse_nextToken = Lens.lens (\GetExternalModelsResponse' {nextToken} -> nextToken) (\s@GetExternalModelsResponse' {} a -> s {nextToken = a} :: GetExternalModelsResponse)

-- | The response's http status code.
getExternalModelsResponse_httpStatus :: Lens.Lens' GetExternalModelsResponse Prelude.Int
getExternalModelsResponse_httpStatus = Lens.lens (\GetExternalModelsResponse' {httpStatus} -> httpStatus) (\s@GetExternalModelsResponse' {} a -> s {httpStatus = a} :: GetExternalModelsResponse)

instance Prelude.NFData GetExternalModelsResponse where
  rnf GetExternalModelsResponse' {..} =
    Prelude.rnf externalModels `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
