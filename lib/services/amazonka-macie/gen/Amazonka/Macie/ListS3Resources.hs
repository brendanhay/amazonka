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
-- Module      : Amazonka.Macie.ListS3Resources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the S3 resources associated with Amazon Macie Classic. If
-- memberAccountId isn\'t specified, the action lists the S3 resources
-- associated with Macie Classic for the current Macie Classic
-- administrator account. If memberAccountId is specified, the action lists
-- the S3 resources associated with Macie Classic for the specified member
-- account.
--
-- This operation returns paginated results.
module Amazonka.Macie.ListS3Resources
  ( -- * Creating a Request
    ListS3Resources (..),
    newListS3Resources,

    -- * Request Lenses
    listS3Resources_memberAccountId,
    listS3Resources_nextToken,
    listS3Resources_maxResults,

    -- * Destructuring the Response
    ListS3ResourcesResponse (..),
    newListS3ResourcesResponse,

    -- * Response Lenses
    listS3ResourcesResponse_nextToken,
    listS3ResourcesResponse_s3Resources,
    listS3ResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Macie.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListS3Resources' smart constructor.
data ListS3Resources = ListS3Resources'
  { -- | The Amazon Macie Classic member account ID whose associated S3 resources
    -- you want to list.
    memberAccountId :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter when paginating results. Set its value to null on
    -- your first call to the ListS3Resources action. Subsequent calls to the
    -- action fill nextToken in the request with the value of nextToken from
    -- the previous response to continue listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to indicate the maximum number of items that you want
    -- in the response. The default value is 250.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListS3Resources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberAccountId', 'listS3Resources_memberAccountId' - The Amazon Macie Classic member account ID whose associated S3 resources
-- you want to list.
--
-- 'nextToken', 'listS3Resources_nextToken' - Use this parameter when paginating results. Set its value to null on
-- your first call to the ListS3Resources action. Subsequent calls to the
-- action fill nextToken in the request with the value of nextToken from
-- the previous response to continue listing data.
--
-- 'maxResults', 'listS3Resources_maxResults' - Use this parameter to indicate the maximum number of items that you want
-- in the response. The default value is 250.
newListS3Resources ::
  ListS3Resources
newListS3Resources =
  ListS3Resources'
    { memberAccountId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The Amazon Macie Classic member account ID whose associated S3 resources
-- you want to list.
listS3Resources_memberAccountId :: Lens.Lens' ListS3Resources (Prelude.Maybe Prelude.Text)
listS3Resources_memberAccountId = Lens.lens (\ListS3Resources' {memberAccountId} -> memberAccountId) (\s@ListS3Resources' {} a -> s {memberAccountId = a} :: ListS3Resources)

-- | Use this parameter when paginating results. Set its value to null on
-- your first call to the ListS3Resources action. Subsequent calls to the
-- action fill nextToken in the request with the value of nextToken from
-- the previous response to continue listing data.
listS3Resources_nextToken :: Lens.Lens' ListS3Resources (Prelude.Maybe Prelude.Text)
listS3Resources_nextToken = Lens.lens (\ListS3Resources' {nextToken} -> nextToken) (\s@ListS3Resources' {} a -> s {nextToken = a} :: ListS3Resources)

-- | Use this parameter to indicate the maximum number of items that you want
-- in the response. The default value is 250.
listS3Resources_maxResults :: Lens.Lens' ListS3Resources (Prelude.Maybe Prelude.Int)
listS3Resources_maxResults = Lens.lens (\ListS3Resources' {maxResults} -> maxResults) (\s@ListS3Resources' {} a -> s {maxResults = a} :: ListS3Resources)

instance Core.AWSPager ListS3Resources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listS3ResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listS3ResourcesResponse_s3Resources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listS3Resources_nextToken
          Lens..~ rs
          Lens.^? listS3ResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListS3Resources where
  type
    AWSResponse ListS3Resources =
      ListS3ResourcesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListS3ResourcesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "s3Resources" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListS3Resources where
  hashWithSalt _salt ListS3Resources' {..} =
    _salt `Prelude.hashWithSalt` memberAccountId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListS3Resources where
  rnf ListS3Resources' {..} =
    Prelude.rnf memberAccountId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListS3Resources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MacieService.ListS3Resources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListS3Resources where
  toJSON ListS3Resources' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("memberAccountId" Core..=)
              Prelude.<$> memberAccountId,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListS3Resources where
  toPath = Prelude.const "/"

instance Core.ToQuery ListS3Resources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListS3ResourcesResponse' smart constructor.
data ListS3ResourcesResponse = ListS3ResourcesResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the nextToken parameter in a subsequent pagination request. If there is
    -- no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the associated S3 resources returned by the action.
    s3Resources :: Prelude.Maybe [S3ResourceClassification],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListS3ResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listS3ResourcesResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
--
-- 's3Resources', 'listS3ResourcesResponse_s3Resources' - A list of the associated S3 resources returned by the action.
--
-- 'httpStatus', 'listS3ResourcesResponse_httpStatus' - The response's http status code.
newListS3ResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListS3ResourcesResponse
newListS3ResourcesResponse pHttpStatus_ =
  ListS3ResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      s3Resources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the nextToken parameter in a subsequent pagination request. If there is
-- no more data to be listed, this parameter is set to null.
listS3ResourcesResponse_nextToken :: Lens.Lens' ListS3ResourcesResponse (Prelude.Maybe Prelude.Text)
listS3ResourcesResponse_nextToken = Lens.lens (\ListS3ResourcesResponse' {nextToken} -> nextToken) (\s@ListS3ResourcesResponse' {} a -> s {nextToken = a} :: ListS3ResourcesResponse)

-- | A list of the associated S3 resources returned by the action.
listS3ResourcesResponse_s3Resources :: Lens.Lens' ListS3ResourcesResponse (Prelude.Maybe [S3ResourceClassification])
listS3ResourcesResponse_s3Resources = Lens.lens (\ListS3ResourcesResponse' {s3Resources} -> s3Resources) (\s@ListS3ResourcesResponse' {} a -> s {s3Resources = a} :: ListS3ResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listS3ResourcesResponse_httpStatus :: Lens.Lens' ListS3ResourcesResponse Prelude.Int
listS3ResourcesResponse_httpStatus = Lens.lens (\ListS3ResourcesResponse' {httpStatus} -> httpStatus) (\s@ListS3ResourcesResponse' {} a -> s {httpStatus = a} :: ListS3ResourcesResponse)

instance Prelude.NFData ListS3ResourcesResponse where
  rnf ListS3ResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf s3Resources
      `Prelude.seq` Prelude.rnf httpStatus
