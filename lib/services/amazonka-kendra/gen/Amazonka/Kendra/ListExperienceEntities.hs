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
-- Module      : Amazonka.Kendra.ListExperienceEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists users or groups in your IAM Identity Center identity source that
-- are granted access to your Amazon Kendra experience. You can create an
-- Amazon Kendra experience such as a search application. For more
-- information on creating a search application experience, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/deploying-search-experience-no-code.html Building a search experience with no code>.
module Amazonka.Kendra.ListExperienceEntities
  ( -- * Creating a Request
    ListExperienceEntities (..),
    newListExperienceEntities,

    -- * Request Lenses
    listExperienceEntities_nextToken,
    listExperienceEntities_id,
    listExperienceEntities_indexId,

    -- * Destructuring the Response
    ListExperienceEntitiesResponse (..),
    newListExperienceEntitiesResponse,

    -- * Response Lenses
    listExperienceEntitiesResponse_nextToken,
    listExperienceEntitiesResponse_summaryItems,
    listExperienceEntitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExperienceEntities' smart constructor.
data ListExperienceEntities = ListExperienceEntities'
  { -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of users or
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of your Amazon Kendra experience.
    id :: Prelude.Text,
    -- | The identifier of the index for your Amazon Kendra experience.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperienceEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperienceEntities_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of users or
-- groups.
--
-- 'id', 'listExperienceEntities_id' - The identifier of your Amazon Kendra experience.
--
-- 'indexId', 'listExperienceEntities_indexId' - The identifier of the index for your Amazon Kendra experience.
newListExperienceEntities ::
  -- | 'id'
  Prelude.Text ->
  -- | 'indexId'
  Prelude.Text ->
  ListExperienceEntities
newListExperienceEntities pId_ pIndexId_ =
  ListExperienceEntities'
    { nextToken =
        Prelude.Nothing,
      id = pId_,
      indexId = pIndexId_
    }

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of users or
-- groups.
listExperienceEntities_nextToken :: Lens.Lens' ListExperienceEntities (Prelude.Maybe Prelude.Text)
listExperienceEntities_nextToken = Lens.lens (\ListExperienceEntities' {nextToken} -> nextToken) (\s@ListExperienceEntities' {} a -> s {nextToken = a} :: ListExperienceEntities)

-- | The identifier of your Amazon Kendra experience.
listExperienceEntities_id :: Lens.Lens' ListExperienceEntities Prelude.Text
listExperienceEntities_id = Lens.lens (\ListExperienceEntities' {id} -> id) (\s@ListExperienceEntities' {} a -> s {id = a} :: ListExperienceEntities)

-- | The identifier of the index for your Amazon Kendra experience.
listExperienceEntities_indexId :: Lens.Lens' ListExperienceEntities Prelude.Text
listExperienceEntities_indexId = Lens.lens (\ListExperienceEntities' {indexId} -> indexId) (\s@ListExperienceEntities' {} a -> s {indexId = a} :: ListExperienceEntities)

instance Core.AWSRequest ListExperienceEntities where
  type
    AWSResponse ListExperienceEntities =
      ListExperienceEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperienceEntitiesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SummaryItems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperienceEntities where
  hashWithSalt _salt ListExperienceEntities' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` indexId

instance Prelude.NFData ListExperienceEntities where
  rnf ListExperienceEntities' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf indexId

instance Data.ToHeaders ListExperienceEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListExperienceEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExperienceEntities where
  toJSON ListExperienceEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListExperienceEntities where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExperienceEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExperienceEntitiesResponse' smart constructor.
data ListExperienceEntitiesResponse = ListExperienceEntitiesResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token, which
    -- you can use in a later request to retrieve the next set of users or
    -- groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of summary information for one or more users or groups.
    summaryItems :: Prelude.Maybe [ExperienceEntitiesSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperienceEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperienceEntitiesResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of users or
-- groups.
--
-- 'summaryItems', 'listExperienceEntitiesResponse_summaryItems' - An array of summary information for one or more users or groups.
--
-- 'httpStatus', 'listExperienceEntitiesResponse_httpStatus' - The response's http status code.
newListExperienceEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExperienceEntitiesResponse
newListExperienceEntitiesResponse pHttpStatus_ =
  ListExperienceEntitiesResponse'
    { nextToken =
        Prelude.Nothing,
      summaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, Amazon Kendra returns this token, which
-- you can use in a later request to retrieve the next set of users or
-- groups.
listExperienceEntitiesResponse_nextToken :: Lens.Lens' ListExperienceEntitiesResponse (Prelude.Maybe Prelude.Text)
listExperienceEntitiesResponse_nextToken = Lens.lens (\ListExperienceEntitiesResponse' {nextToken} -> nextToken) (\s@ListExperienceEntitiesResponse' {} a -> s {nextToken = a} :: ListExperienceEntitiesResponse)

-- | An array of summary information for one or more users or groups.
listExperienceEntitiesResponse_summaryItems :: Lens.Lens' ListExperienceEntitiesResponse (Prelude.Maybe [ExperienceEntitiesSummary])
listExperienceEntitiesResponse_summaryItems = Lens.lens (\ListExperienceEntitiesResponse' {summaryItems} -> summaryItems) (\s@ListExperienceEntitiesResponse' {} a -> s {summaryItems = a} :: ListExperienceEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listExperienceEntitiesResponse_httpStatus :: Lens.Lens' ListExperienceEntitiesResponse Prelude.Int
listExperienceEntitiesResponse_httpStatus = Lens.lens (\ListExperienceEntitiesResponse' {httpStatus} -> httpStatus) (\s@ListExperienceEntitiesResponse' {} a -> s {httpStatus = a} :: ListExperienceEntitiesResponse)

instance
  Prelude.NFData
    ListExperienceEntitiesResponse
  where
  rnf ListExperienceEntitiesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf summaryItems `Prelude.seq`
        Prelude.rnf httpStatus
