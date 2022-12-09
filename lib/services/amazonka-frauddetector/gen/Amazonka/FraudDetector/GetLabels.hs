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
-- Module      : Amazonka.FraudDetector.GetLabels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all labels or a specific label if name is provided. This is a
-- paginated API. If you provide a null @maxResults@, this action retrieves
-- a maximum of 50 records per page. If you provide a @maxResults@, the
-- value must be between 10 and 50. To get the next page results, provide
-- the pagination token from the @GetGetLabelsResponse@ as part of your
-- request. A null pagination token fetches the records from the beginning.
module Amazonka.FraudDetector.GetLabels
  ( -- * Creating a Request
    GetLabels (..),
    newGetLabels,

    -- * Request Lenses
    getLabels_maxResults,
    getLabels_name,
    getLabels_nextToken,

    -- * Destructuring the Response
    GetLabelsResponse (..),
    newGetLabelsResponse,

    -- * Response Lenses
    getLabelsResponse_labels,
    getLabelsResponse_nextToken,
    getLabelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLabels' smart constructor.
data GetLabels = GetLabels'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the label or labels to get.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLabels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getLabels_maxResults' - The maximum number of objects to return for the request.
--
-- 'name', 'getLabels_name' - The name of the label or labels to get.
--
-- 'nextToken', 'getLabels_nextToken' - The next token for the subsequent request.
newGetLabels ::
  GetLabels
newGetLabels =
  GetLabels'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects to return for the request.
getLabels_maxResults :: Lens.Lens' GetLabels (Prelude.Maybe Prelude.Natural)
getLabels_maxResults = Lens.lens (\GetLabels' {maxResults} -> maxResults) (\s@GetLabels' {} a -> s {maxResults = a} :: GetLabels)

-- | The name of the label or labels to get.
getLabels_name :: Lens.Lens' GetLabels (Prelude.Maybe Prelude.Text)
getLabels_name = Lens.lens (\GetLabels' {name} -> name) (\s@GetLabels' {} a -> s {name = a} :: GetLabels)

-- | The next token for the subsequent request.
getLabels_nextToken :: Lens.Lens' GetLabels (Prelude.Maybe Prelude.Text)
getLabels_nextToken = Lens.lens (\GetLabels' {nextToken} -> nextToken) (\s@GetLabels' {} a -> s {nextToken = a} :: GetLabels)

instance Core.AWSRequest GetLabels where
  type AWSResponse GetLabels = GetLabelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLabelsResponse'
            Prelude.<$> (x Data..?> "labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLabels where
  hashWithSalt _salt GetLabels' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetLabels where
  rnf GetLabels' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetLabels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetLabels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLabels where
  toJSON GetLabels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetLabels where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLabels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLabelsResponse' smart constructor.
data GetLabelsResponse = GetLabelsResponse'
  { -- | An array of labels.
    labels :: Prelude.Maybe [Label],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLabelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'getLabelsResponse_labels' - An array of labels.
--
-- 'nextToken', 'getLabelsResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getLabelsResponse_httpStatus' - The response's http status code.
newGetLabelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLabelsResponse
newGetLabelsResponse pHttpStatus_ =
  GetLabelsResponse'
    { labels = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of labels.
getLabelsResponse_labels :: Lens.Lens' GetLabelsResponse (Prelude.Maybe [Label])
getLabelsResponse_labels = Lens.lens (\GetLabelsResponse' {labels} -> labels) (\s@GetLabelsResponse' {} a -> s {labels = a} :: GetLabelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getLabelsResponse_nextToken :: Lens.Lens' GetLabelsResponse (Prelude.Maybe Prelude.Text)
getLabelsResponse_nextToken = Lens.lens (\GetLabelsResponse' {nextToken} -> nextToken) (\s@GetLabelsResponse' {} a -> s {nextToken = a} :: GetLabelsResponse)

-- | The response's http status code.
getLabelsResponse_httpStatus :: Lens.Lens' GetLabelsResponse Prelude.Int
getLabelsResponse_httpStatus = Lens.lens (\GetLabelsResponse' {httpStatus} -> httpStatus) (\s@GetLabelsResponse' {} a -> s {httpStatus = a} :: GetLabelsResponse)

instance Prelude.NFData GetLabelsResponse where
  rnf GetLabelsResponse' {..} =
    Prelude.rnf labels
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
