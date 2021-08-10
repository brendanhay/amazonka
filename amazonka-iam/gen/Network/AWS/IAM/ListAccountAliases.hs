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
-- Module      : Network.AWS.IAM.ListAccountAliases
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the account alias associated with the AWS account (Note: you can
-- have only one). For information about using an AWS account alias, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an alias for your AWS account ID>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccountAliases
  ( -- * Creating a Request
    ListAccountAliases (..),
    newListAccountAliases,

    -- * Request Lenses
    listAccountAliases_maxItems,
    listAccountAliases_marker,

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

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAccountAliases' smart constructor.
data ListAccountAliases = ListAccountAliases'
  { -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'maxItems', 'listAccountAliases_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listAccountAliases_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListAccountAliases ::
  ListAccountAliases
newListAccountAliases =
  ListAccountAliases'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

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

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAccountAliases_marker :: Lens.Lens' ListAccountAliases (Prelude.Maybe Prelude.Text)
listAccountAliases_marker = Lens.lens (\ListAccountAliases' {marker} -> marker) (\s@ListAccountAliases' {} a -> s {marker = a} :: ListAccountAliases)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAccountAliasesResult"
      ( \s h x ->
          ListAccountAliasesResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "AccountAliases" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListAccountAliases

instance Prelude.NFData ListAccountAliases

instance Core.ToHeaders ListAccountAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAccountAliases where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAccountAliases where
  toQuery ListAccountAliases' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListAccountAliases" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
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
    -- | A list of aliases associated with the account. AWS supports only one
    -- alias per account.
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
-- 'accountAliases', 'listAccountAliasesResponse_accountAliases' - A list of aliases associated with the account. AWS supports only one
-- alias per account.
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

-- | A list of aliases associated with the account. AWS supports only one
-- alias per account.
listAccountAliasesResponse_accountAliases :: Lens.Lens' ListAccountAliasesResponse [Prelude.Text]
listAccountAliasesResponse_accountAliases = Lens.lens (\ListAccountAliasesResponse' {accountAliases} -> accountAliases) (\s@ListAccountAliasesResponse' {} a -> s {accountAliases = a} :: ListAccountAliasesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListAccountAliasesResponse
