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
-- Module      : Amazonka.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account alias associated with the Amazon Web Services account
-- (Note: you can have only one). For information about using an Amazon Web
-- Services account alias, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an alias for your Amazon Web Services account ID>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListAccountAliases
  ( -- * Creating a Request
    ListAccountAliases (..),
    newListAccountAliases,

    -- * Request Lenses
    listAccountAliases_marker,
    listAccountAliases_maxItems,

    -- * Destructuring the Response
    ListAccountAliasesResponse (..),
    newListAccountAliasesResponse,

    -- * Response Lenses
    listAccountAliasesResponse_isTruncated,
    listAccountAliasesResponse_marker,
    listAccountAliasesResponse_httpStatus,
    listAccountAliasesResponse_accountAliases,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
  { -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listAccountAliases_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listAccountAliases_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
newListAccountAliases ::
  ListAccountAliases
newListAccountAliases =
  ListAccountAliases'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAccountAliases_marker :: Lens.Lens' ListAccountAliases (Prelude.Maybe Prelude.Text)
listAccountAliases_marker = Lens.lens (\ListAccountAliases' {marker} -> marker) (\s@ListAccountAliases' {} a -> s {marker = a} :: ListAccountAliases)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAccountAliases_maxItems :: Lens.Lens' ListAccountAliases (Prelude.Maybe Prelude.Natural)
listAccountAliases_maxItems = Lens.lens (\ListAccountAliases' {maxItems} -> maxItems) (\s@ListAccountAliases' {} a -> s {maxItems = a} :: ListAccountAliases)

instance Core.AWSPager ListAccountAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAccountAliasesResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listAccountAliasesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAccountAliases_marker
              Lens..~ rs
              Lens.^? listAccountAliasesResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest ListAccountAliases where
  type
    AWSResponse ListAccountAliases =
      ListAccountAliasesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListAccountAliasesResult"
      ( \s h x ->
          ListAccountAliasesResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "AccountAliases" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListAccountAliases where
  hashWithSalt _salt ListAccountAliases' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems

instance Prelude.NFData ListAccountAliases where
  rnf ListAccountAliases' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems

instance Data.ToHeaders ListAccountAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAccountAliases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAccountAliases where
  toQuery ListAccountAliases' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListAccountAliases" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems
      ]

-- | Contains the response to a successful ListAccountAliases request.
--
-- /See:/ 'newListAccountAliasesResponse' smart constructor.
data ListAccountAliasesResponse = ListAccountAliasesResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of aliases associated with the account. Amazon Web Services
    -- supports only one alias per account.
    accountAliases :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccountAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listAccountAliasesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listAccountAliasesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAccountAliasesResponse_httpStatus' - The response's http status code.
--
-- 'accountAliases', 'listAccountAliasesResponse_accountAliases' - A list of aliases associated with the account. Amazon Web Services
-- supports only one alias per account.
newListAccountAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccountAliasesResponse
newListAccountAliasesResponse pHttpStatus_ =
  ListAccountAliasesResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      accountAliases = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAccountAliasesResponse_isTruncated :: Lens.Lens' ListAccountAliasesResponse (Prelude.Maybe Prelude.Bool)
listAccountAliasesResponse_isTruncated = Lens.lens (\ListAccountAliasesResponse' {isTruncated} -> isTruncated) (\s@ListAccountAliasesResponse' {} a -> s {isTruncated = a} :: ListAccountAliasesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAccountAliasesResponse_marker :: Lens.Lens' ListAccountAliasesResponse (Prelude.Maybe Prelude.Text)
listAccountAliasesResponse_marker = Lens.lens (\ListAccountAliasesResponse' {marker} -> marker) (\s@ListAccountAliasesResponse' {} a -> s {marker = a} :: ListAccountAliasesResponse)

-- | The response's http status code.
listAccountAliasesResponse_httpStatus :: Lens.Lens' ListAccountAliasesResponse Prelude.Int
listAccountAliasesResponse_httpStatus = Lens.lens (\ListAccountAliasesResponse' {httpStatus} -> httpStatus) (\s@ListAccountAliasesResponse' {} a -> s {httpStatus = a} :: ListAccountAliasesResponse)

-- | A list of aliases associated with the account. Amazon Web Services
-- supports only one alias per account.
listAccountAliasesResponse_accountAliases :: Lens.Lens' ListAccountAliasesResponse [Prelude.Text]
listAccountAliasesResponse_accountAliases = Lens.lens (\ListAccountAliasesResponse' {accountAliases} -> accountAliases) (\s@ListAccountAliasesResponse' {} a -> s {accountAliases = a} :: ListAccountAliasesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAccountAliasesResponse where
  rnf ListAccountAliasesResponse' {..} =
    Prelude.rnf isTruncated `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf accountAliases
