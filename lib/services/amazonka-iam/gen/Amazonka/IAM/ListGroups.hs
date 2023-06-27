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
-- Module      : Amazonka.IAM.ListGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that have the specified path prefix.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListGroups
  ( -- * Creating a Request
    ListGroups (..),
    newListGroups,

    -- * Request Lenses
    listGroups_marker,
    listGroups_maxItems,
    listGroups_pathPrefix,

    -- * Destructuring the Response
    ListGroupsResponse (..),
    newListGroupsResponse,

    -- * Response Lenses
    listGroupsResponse_isTruncated,
    listGroupsResponse_marker,
    listGroupsResponse_httpStatus,
    listGroupsResponse_groups,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroups' smart constructor.
data ListGroups = ListGroups'
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
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | The path prefix for filtering the results. For example, the prefix
    -- @\/division_abc\/subdivision_xyz\/@ gets all groups whose path starts
    -- with @\/division_abc\/subdivision_xyz\/@.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/), listing all groups. This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    pathPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listGroups_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listGroups_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'pathPrefix', 'listGroups_pathPrefix' - The path prefix for filtering the results. For example, the prefix
-- @\/division_abc\/subdivision_xyz\/@ gets all groups whose path starts
-- with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all groups. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
newListGroups ::
  ListGroups
newListGroups =
  ListGroups'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      pathPrefix = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listGroups_marker :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Text)
listGroups_marker = Lens.lens (\ListGroups' {marker} -> marker) (\s@ListGroups' {} a -> s {marker = a} :: ListGroups)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listGroups_maxItems :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Natural)
listGroups_maxItems = Lens.lens (\ListGroups' {maxItems} -> maxItems) (\s@ListGroups' {} a -> s {maxItems = a} :: ListGroups)

-- | The path prefix for filtering the results. For example, the prefix
-- @\/division_abc\/subdivision_xyz\/@ gets all groups whose path starts
-- with @\/division_abc\/subdivision_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all groups. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
listGroups_pathPrefix :: Lens.Lens' ListGroups (Prelude.Maybe Prelude.Text)
listGroups_pathPrefix = Lens.lens (\ListGroups' {pathPrefix} -> pathPrefix) (\s@ListGroups' {} a -> s {pathPrefix = a} :: ListGroups)

instance Core.AWSPager ListGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupsResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listGroupsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listGroups_marker
          Lens..~ rs
          Lens.^? listGroupsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListGroups where
  type AWSResponse ListGroups = ListGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListGroupsResult"
      ( \s h x ->
          ListGroupsResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "Groups"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListGroups where
  hashWithSalt _salt ListGroups' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` pathPrefix

instance Prelude.NFData ListGroups where
  rnf ListGroups' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf pathPrefix

instance Data.ToHeaders ListGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGroups where
  toQuery ListGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PathPrefix" Data.=: pathPrefix
      ]

-- | Contains the response to a successful ListGroups request.
--
-- /See:/ 'newListGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
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
    -- | A list of groups.
    groups :: [Group]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listGroupsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listGroupsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listGroupsResponse_httpStatus' - The response's http status code.
--
-- 'groups', 'listGroupsResponse_groups' - A list of groups.
newListGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsResponse
newListGroupsResponse pHttpStatus_ =
  ListGroupsResponse'
    { isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      groups = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listGroupsResponse_isTruncated :: Lens.Lens' ListGroupsResponse (Prelude.Maybe Prelude.Bool)
listGroupsResponse_isTruncated = Lens.lens (\ListGroupsResponse' {isTruncated} -> isTruncated) (\s@ListGroupsResponse' {} a -> s {isTruncated = a} :: ListGroupsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listGroupsResponse_marker :: Lens.Lens' ListGroupsResponse (Prelude.Maybe Prelude.Text)
listGroupsResponse_marker = Lens.lens (\ListGroupsResponse' {marker} -> marker) (\s@ListGroupsResponse' {} a -> s {marker = a} :: ListGroupsResponse)

-- | The response's http status code.
listGroupsResponse_httpStatus :: Lens.Lens' ListGroupsResponse Prelude.Int
listGroupsResponse_httpStatus = Lens.lens (\ListGroupsResponse' {httpStatus} -> httpStatus) (\s@ListGroupsResponse' {} a -> s {httpStatus = a} :: ListGroupsResponse)

-- | A list of groups.
listGroupsResponse_groups :: Lens.Lens' ListGroupsResponse [Group]
listGroupsResponse_groups = Lens.lens (\ListGroupsResponse' {groups} -> groups) (\s@ListGroupsResponse' {} a -> s {groups = a} :: ListGroupsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListGroupsResponse where
  rnf ListGroupsResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf groups
