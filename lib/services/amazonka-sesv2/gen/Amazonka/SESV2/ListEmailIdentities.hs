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
-- Module      : Amazonka.SESV2.ListEmailIdentities
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all of the email identities that are associated with
-- your Amazon Web Services account. An identity can be either an email
-- address or a domain. This operation returns identities that are verified
-- as well as those that aren\'t. This operation returns identities that
-- are associated with Amazon SES and Amazon Pinpoint.
module Amazonka.SESV2.ListEmailIdentities
  ( -- * Creating a Request
    ListEmailIdentities (..),
    newListEmailIdentities,

    -- * Request Lenses
    listEmailIdentities_nextToken,
    listEmailIdentities_pageSize,

    -- * Destructuring the Response
    ListEmailIdentitiesResponse (..),
    newListEmailIdentitiesResponse,

    -- * Response Lenses
    listEmailIdentitiesResponse_nextToken,
    listEmailIdentitiesResponse_emailIdentities,
    listEmailIdentitiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to list all of the email identities associated with your
-- Amazon Web Services account. This list includes identities that you\'ve
-- already verified, identities that are unverified, and identities that
-- were verified in the past, but are no longer verified.
--
-- /See:/ 'newListEmailIdentities' smart constructor.
data ListEmailIdentities = ListEmailIdentities'
  { -- | A token returned from a previous call to @ListEmailIdentities@ to
    -- indicate the position in the list of identities.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to @ListEmailIdentities@.
    -- If the number of results is larger than the number you specified in this
    -- parameter, then the response includes a @NextToken@ element, which you
    -- can use to obtain additional results.
    --
    -- The value you specify has to be at least 0, and can be no more than
    -- 1000.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEmailIdentities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEmailIdentities_nextToken' - A token returned from a previous call to @ListEmailIdentities@ to
-- indicate the position in the list of identities.
--
-- 'pageSize', 'listEmailIdentities_pageSize' - The number of results to show in a single call to @ListEmailIdentities@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 0, and can be no more than
-- 1000.
newListEmailIdentities ::
  ListEmailIdentities
newListEmailIdentities =
  ListEmailIdentities'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListEmailIdentities@ to
-- indicate the position in the list of identities.
listEmailIdentities_nextToken :: Lens.Lens' ListEmailIdentities (Prelude.Maybe Prelude.Text)
listEmailIdentities_nextToken = Lens.lens (\ListEmailIdentities' {nextToken} -> nextToken) (\s@ListEmailIdentities' {} a -> s {nextToken = a} :: ListEmailIdentities)

-- | The number of results to show in a single call to @ListEmailIdentities@.
-- If the number of results is larger than the number you specified in this
-- parameter, then the response includes a @NextToken@ element, which you
-- can use to obtain additional results.
--
-- The value you specify has to be at least 0, and can be no more than
-- 1000.
listEmailIdentities_pageSize :: Lens.Lens' ListEmailIdentities (Prelude.Maybe Prelude.Int)
listEmailIdentities_pageSize = Lens.lens (\ListEmailIdentities' {pageSize} -> pageSize) (\s@ListEmailIdentities' {} a -> s {pageSize = a} :: ListEmailIdentities)

instance Core.AWSRequest ListEmailIdentities where
  type
    AWSResponse ListEmailIdentities =
      ListEmailIdentitiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEmailIdentitiesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "EmailIdentities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEmailIdentities where
  hashWithSalt _salt ListEmailIdentities' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListEmailIdentities where
  rnf ListEmailIdentities' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListEmailIdentities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEmailIdentities where
  toPath = Prelude.const "/v2/email/identities"

instance Data.ToQuery ListEmailIdentities where
  toQuery ListEmailIdentities' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | A list of all of the identities that you\'ve attempted to verify,
-- regardless of whether or not those identities were successfully
-- verified.
--
-- /See:/ 'newListEmailIdentitiesResponse' smart constructor.
data ListEmailIdentitiesResponse = ListEmailIdentitiesResponse'
  { -- | A token that indicates that there are additional configuration sets to
    -- list. To view additional configuration sets, issue another request to
    -- @ListEmailIdentities@, and pass this token in the @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array that includes all of the email identities associated with your
    -- Amazon Web Services account.
    emailIdentities :: Prelude.Maybe [IdentityInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEmailIdentitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEmailIdentitiesResponse_nextToken' - A token that indicates that there are additional configuration sets to
-- list. To view additional configuration sets, issue another request to
-- @ListEmailIdentities@, and pass this token in the @NextToken@ parameter.
--
-- 'emailIdentities', 'listEmailIdentitiesResponse_emailIdentities' - An array that includes all of the email identities associated with your
-- Amazon Web Services account.
--
-- 'httpStatus', 'listEmailIdentitiesResponse_httpStatus' - The response's http status code.
newListEmailIdentitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEmailIdentitiesResponse
newListEmailIdentitiesResponse pHttpStatus_ =
  ListEmailIdentitiesResponse'
    { nextToken =
        Prelude.Nothing,
      emailIdentities = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that indicates that there are additional configuration sets to
-- list. To view additional configuration sets, issue another request to
-- @ListEmailIdentities@, and pass this token in the @NextToken@ parameter.
listEmailIdentitiesResponse_nextToken :: Lens.Lens' ListEmailIdentitiesResponse (Prelude.Maybe Prelude.Text)
listEmailIdentitiesResponse_nextToken = Lens.lens (\ListEmailIdentitiesResponse' {nextToken} -> nextToken) (\s@ListEmailIdentitiesResponse' {} a -> s {nextToken = a} :: ListEmailIdentitiesResponse)

-- | An array that includes all of the email identities associated with your
-- Amazon Web Services account.
listEmailIdentitiesResponse_emailIdentities :: Lens.Lens' ListEmailIdentitiesResponse (Prelude.Maybe [IdentityInfo])
listEmailIdentitiesResponse_emailIdentities = Lens.lens (\ListEmailIdentitiesResponse' {emailIdentities} -> emailIdentities) (\s@ListEmailIdentitiesResponse' {} a -> s {emailIdentities = a} :: ListEmailIdentitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEmailIdentitiesResponse_httpStatus :: Lens.Lens' ListEmailIdentitiesResponse Prelude.Int
listEmailIdentitiesResponse_httpStatus = Lens.lens (\ListEmailIdentitiesResponse' {httpStatus} -> httpStatus) (\s@ListEmailIdentitiesResponse' {} a -> s {httpStatus = a} :: ListEmailIdentitiesResponse)

instance Prelude.NFData ListEmailIdentitiesResponse where
  rnf ListEmailIdentitiesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf emailIdentities
      `Prelude.seq` Prelude.rnf httpStatus
