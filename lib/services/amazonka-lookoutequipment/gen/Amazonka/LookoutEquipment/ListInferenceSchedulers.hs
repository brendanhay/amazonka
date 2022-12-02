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
-- Module      : Amazonka.LookoutEquipment.ListInferenceSchedulers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all inference schedulers currently available for
-- your account.
module Amazonka.LookoutEquipment.ListInferenceSchedulers
  ( -- * Creating a Request
    ListInferenceSchedulers (..),
    newListInferenceSchedulers,

    -- * Request Lenses
    listInferenceSchedulers_nextToken,
    listInferenceSchedulers_inferenceSchedulerNameBeginsWith,
    listInferenceSchedulers_maxResults,
    listInferenceSchedulers_modelName,

    -- * Destructuring the Response
    ListInferenceSchedulersResponse (..),
    newListInferenceSchedulersResponse,

    -- * Response Lenses
    listInferenceSchedulersResponse_nextToken,
    listInferenceSchedulersResponse_inferenceSchedulerSummaries,
    listInferenceSchedulersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInferenceSchedulers' smart constructor.
data ListInferenceSchedulers = ListInferenceSchedulers'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- inference schedulers.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The beginning of the name of the inference schedulers to be listed.
    inferenceSchedulerNameBeginsWith :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of inference schedulers to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the ML model used by the inference scheduler to be listed.
    modelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceSchedulers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInferenceSchedulers_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference schedulers.
--
-- 'inferenceSchedulerNameBeginsWith', 'listInferenceSchedulers_inferenceSchedulerNameBeginsWith' - The beginning of the name of the inference schedulers to be listed.
--
-- 'maxResults', 'listInferenceSchedulers_maxResults' - Specifies the maximum number of inference schedulers to list.
--
-- 'modelName', 'listInferenceSchedulers_modelName' - The name of the ML model used by the inference scheduler to be listed.
newListInferenceSchedulers ::
  ListInferenceSchedulers
newListInferenceSchedulers =
  ListInferenceSchedulers'
    { nextToken =
        Prelude.Nothing,
      inferenceSchedulerNameBeginsWith = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelName = Prelude.Nothing
    }

-- | An opaque pagination token indicating where to continue the listing of
-- inference schedulers.
listInferenceSchedulers_nextToken :: Lens.Lens' ListInferenceSchedulers (Prelude.Maybe Prelude.Text)
listInferenceSchedulers_nextToken = Lens.lens (\ListInferenceSchedulers' {nextToken} -> nextToken) (\s@ListInferenceSchedulers' {} a -> s {nextToken = a} :: ListInferenceSchedulers)

-- | The beginning of the name of the inference schedulers to be listed.
listInferenceSchedulers_inferenceSchedulerNameBeginsWith :: Lens.Lens' ListInferenceSchedulers (Prelude.Maybe Prelude.Text)
listInferenceSchedulers_inferenceSchedulerNameBeginsWith = Lens.lens (\ListInferenceSchedulers' {inferenceSchedulerNameBeginsWith} -> inferenceSchedulerNameBeginsWith) (\s@ListInferenceSchedulers' {} a -> s {inferenceSchedulerNameBeginsWith = a} :: ListInferenceSchedulers)

-- | Specifies the maximum number of inference schedulers to list.
listInferenceSchedulers_maxResults :: Lens.Lens' ListInferenceSchedulers (Prelude.Maybe Prelude.Natural)
listInferenceSchedulers_maxResults = Lens.lens (\ListInferenceSchedulers' {maxResults} -> maxResults) (\s@ListInferenceSchedulers' {} a -> s {maxResults = a} :: ListInferenceSchedulers)

-- | The name of the ML model used by the inference scheduler to be listed.
listInferenceSchedulers_modelName :: Lens.Lens' ListInferenceSchedulers (Prelude.Maybe Prelude.Text)
listInferenceSchedulers_modelName = Lens.lens (\ListInferenceSchedulers' {modelName} -> modelName) (\s@ListInferenceSchedulers' {} a -> s {modelName = a} :: ListInferenceSchedulers)

instance Core.AWSRequest ListInferenceSchedulers where
  type
    AWSResponse ListInferenceSchedulers =
      ListInferenceSchedulersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceSchedulersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "InferenceSchedulerSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInferenceSchedulers where
  hashWithSalt _salt ListInferenceSchedulers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` inferenceSchedulerNameBeginsWith
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData ListInferenceSchedulers where
  rnf ListInferenceSchedulers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf inferenceSchedulerNameBeginsWith
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelName

instance Data.ToHeaders ListInferenceSchedulers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListInferenceSchedulers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInferenceSchedulers where
  toJSON ListInferenceSchedulers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("InferenceSchedulerNameBeginsWith" Data..=)
              Prelude.<$> inferenceSchedulerNameBeginsWith,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelName" Data..=) Prelude.<$> modelName
          ]
      )

instance Data.ToPath ListInferenceSchedulers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInferenceSchedulers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceSchedulersResponse' smart constructor.
data ListInferenceSchedulersResponse = ListInferenceSchedulersResponse'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- inference schedulers.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the specified inference scheduler, including
    -- data upload frequency, model name and ARN, and status.
    inferenceSchedulerSummaries :: Prelude.Maybe [InferenceSchedulerSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceSchedulersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInferenceSchedulersResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- inference schedulers.
--
-- 'inferenceSchedulerSummaries', 'listInferenceSchedulersResponse_inferenceSchedulerSummaries' - Provides information about the specified inference scheduler, including
-- data upload frequency, model name and ARN, and status.
--
-- 'httpStatus', 'listInferenceSchedulersResponse_httpStatus' - The response's http status code.
newListInferenceSchedulersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceSchedulersResponse
newListInferenceSchedulersResponse pHttpStatus_ =
  ListInferenceSchedulersResponse'
    { nextToken =
        Prelude.Nothing,
      inferenceSchedulerSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token indicating where to continue the listing of
-- inference schedulers.
listInferenceSchedulersResponse_nextToken :: Lens.Lens' ListInferenceSchedulersResponse (Prelude.Maybe Prelude.Text)
listInferenceSchedulersResponse_nextToken = Lens.lens (\ListInferenceSchedulersResponse' {nextToken} -> nextToken) (\s@ListInferenceSchedulersResponse' {} a -> s {nextToken = a} :: ListInferenceSchedulersResponse)

-- | Provides information about the specified inference scheduler, including
-- data upload frequency, model name and ARN, and status.
listInferenceSchedulersResponse_inferenceSchedulerSummaries :: Lens.Lens' ListInferenceSchedulersResponse (Prelude.Maybe [InferenceSchedulerSummary])
listInferenceSchedulersResponse_inferenceSchedulerSummaries = Lens.lens (\ListInferenceSchedulersResponse' {inferenceSchedulerSummaries} -> inferenceSchedulerSummaries) (\s@ListInferenceSchedulersResponse' {} a -> s {inferenceSchedulerSummaries = a} :: ListInferenceSchedulersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInferenceSchedulersResponse_httpStatus :: Lens.Lens' ListInferenceSchedulersResponse Prelude.Int
listInferenceSchedulersResponse_httpStatus = Lens.lens (\ListInferenceSchedulersResponse' {httpStatus} -> httpStatus) (\s@ListInferenceSchedulersResponse' {} a -> s {httpStatus = a} :: ListInferenceSchedulersResponse)

instance
  Prelude.NFData
    ListInferenceSchedulersResponse
  where
  rnf ListInferenceSchedulersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf inferenceSchedulerSummaries
      `Prelude.seq` Prelude.rnf httpStatus
