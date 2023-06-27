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
-- Module      : Amazonka.S3Outposts.ListOutpostsWithS3
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Outposts with S3 on Outposts capacity for your Amazon Web
-- Services account. Includes S3 on Outposts that you have access to as the
-- Outposts owner, or as a shared user from Resource Access Manager (RAM).
--
-- This operation returns paginated results.
module Amazonka.S3Outposts.ListOutpostsWithS3
  ( -- * Creating a Request
    ListOutpostsWithS3 (..),
    newListOutpostsWithS3,

    -- * Request Lenses
    listOutpostsWithS3_maxResults,
    listOutpostsWithS3_nextToken,

    -- * Destructuring the Response
    ListOutpostsWithS3Response (..),
    newListOutpostsWithS3Response,

    -- * Response Lenses
    listOutpostsWithS3Response_nextToken,
    listOutpostsWithS3Response_outposts,
    listOutpostsWithS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3Outposts.Types

-- | /See:/ 'newListOutpostsWithS3' smart constructor.
data ListOutpostsWithS3 = ListOutpostsWithS3'
  { -- | The maximum number of Outposts to return. The limit is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you can get additional results from the @ListOutpostsWithS3@ call,
    -- a @NextToken@ parameter is returned in the output. You can then pass in
    -- a subsequent command to the @NextToken@ parameter to continue listing
    -- additional Outposts.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutpostsWithS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOutpostsWithS3_maxResults' - The maximum number of Outposts to return. The limit is 100.
--
-- 'nextToken', 'listOutpostsWithS3_nextToken' - When you can get additional results from the @ListOutpostsWithS3@ call,
-- a @NextToken@ parameter is returned in the output. You can then pass in
-- a subsequent command to the @NextToken@ parameter to continue listing
-- additional Outposts.
newListOutpostsWithS3 ::
  ListOutpostsWithS3
newListOutpostsWithS3 =
  ListOutpostsWithS3'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of Outposts to return. The limit is 100.
listOutpostsWithS3_maxResults :: Lens.Lens' ListOutpostsWithS3 (Prelude.Maybe Prelude.Natural)
listOutpostsWithS3_maxResults = Lens.lens (\ListOutpostsWithS3' {maxResults} -> maxResults) (\s@ListOutpostsWithS3' {} a -> s {maxResults = a} :: ListOutpostsWithS3)

-- | When you can get additional results from the @ListOutpostsWithS3@ call,
-- a @NextToken@ parameter is returned in the output. You can then pass in
-- a subsequent command to the @NextToken@ parameter to continue listing
-- additional Outposts.
listOutpostsWithS3_nextToken :: Lens.Lens' ListOutpostsWithS3 (Prelude.Maybe Prelude.Text)
listOutpostsWithS3_nextToken = Lens.lens (\ListOutpostsWithS3' {nextToken} -> nextToken) (\s@ListOutpostsWithS3' {} a -> s {nextToken = a} :: ListOutpostsWithS3)

instance Core.AWSPager ListOutpostsWithS3 where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOutpostsWithS3Response_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOutpostsWithS3Response_outposts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listOutpostsWithS3_nextToken
          Lens..~ rs
          Lens.^? listOutpostsWithS3Response_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListOutpostsWithS3 where
  type
    AWSResponse ListOutpostsWithS3 =
      ListOutpostsWithS3Response
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOutpostsWithS3Response'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Outposts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOutpostsWithS3 where
  hashWithSalt _salt ListOutpostsWithS3' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListOutpostsWithS3 where
  rnf ListOutpostsWithS3' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListOutpostsWithS3 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListOutpostsWithS3 where
  toPath =
    Prelude.const "/S3Outposts/ListOutpostsWithS3"

instance Data.ToQuery ListOutpostsWithS3 where
  toQuery ListOutpostsWithS3' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListOutpostsWithS3Response' smart constructor.
data ListOutpostsWithS3Response = ListOutpostsWithS3Response'
  { -- | Returns a token that you can use to call @ListOutpostsWithS3@ again and
    -- receive additional results, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns the list of Outposts that have the following characteristics:
    --
    -- -   outposts that have S3 provisioned
    --
    -- -   outposts that are @Active@ (not pending any provisioning nor
    --     decommissioned)
    --
    -- -   outposts to which the the calling Amazon Web Services account has
    --     access
    outposts :: Prelude.Maybe [Outpost],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOutpostsWithS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOutpostsWithS3Response_nextToken' - Returns a token that you can use to call @ListOutpostsWithS3@ again and
-- receive additional results, if there are any.
--
-- 'outposts', 'listOutpostsWithS3Response_outposts' - Returns the list of Outposts that have the following characteristics:
--
-- -   outposts that have S3 provisioned
--
-- -   outposts that are @Active@ (not pending any provisioning nor
--     decommissioned)
--
-- -   outposts to which the the calling Amazon Web Services account has
--     access
--
-- 'httpStatus', 'listOutpostsWithS3Response_httpStatus' - The response's http status code.
newListOutpostsWithS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOutpostsWithS3Response
newListOutpostsWithS3Response pHttpStatus_ =
  ListOutpostsWithS3Response'
    { nextToken =
        Prelude.Nothing,
      outposts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a token that you can use to call @ListOutpostsWithS3@ again and
-- receive additional results, if there are any.
listOutpostsWithS3Response_nextToken :: Lens.Lens' ListOutpostsWithS3Response (Prelude.Maybe Prelude.Text)
listOutpostsWithS3Response_nextToken = Lens.lens (\ListOutpostsWithS3Response' {nextToken} -> nextToken) (\s@ListOutpostsWithS3Response' {} a -> s {nextToken = a} :: ListOutpostsWithS3Response)

-- | Returns the list of Outposts that have the following characteristics:
--
-- -   outposts that have S3 provisioned
--
-- -   outposts that are @Active@ (not pending any provisioning nor
--     decommissioned)
--
-- -   outposts to which the the calling Amazon Web Services account has
--     access
listOutpostsWithS3Response_outposts :: Lens.Lens' ListOutpostsWithS3Response (Prelude.Maybe [Outpost])
listOutpostsWithS3Response_outposts = Lens.lens (\ListOutpostsWithS3Response' {outposts} -> outposts) (\s@ListOutpostsWithS3Response' {} a -> s {outposts = a} :: ListOutpostsWithS3Response) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOutpostsWithS3Response_httpStatus :: Lens.Lens' ListOutpostsWithS3Response Prelude.Int
listOutpostsWithS3Response_httpStatus = Lens.lens (\ListOutpostsWithS3Response' {httpStatus} -> httpStatus) (\s@ListOutpostsWithS3Response' {} a -> s {httpStatus = a} :: ListOutpostsWithS3Response)

instance Prelude.NFData ListOutpostsWithS3Response where
  rnf ListOutpostsWithS3Response' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf outposts
      `Prelude.seq` Prelude.rnf httpStatus
