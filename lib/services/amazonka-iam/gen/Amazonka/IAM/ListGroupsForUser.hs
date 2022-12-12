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
-- Module      : Amazonka.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM groups that the specified IAM user belongs to.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListGroupsForUser
  ( -- * Creating a Request
    ListGroupsForUser (..),
    newListGroupsForUser,

    -- * Request Lenses
    listGroupsForUser_marker,
    listGroupsForUser_maxItems,
    listGroupsForUser_userName,

    -- * Destructuring the Response
    ListGroupsForUserResponse (..),
    newListGroupsForUserResponse,

    -- * Response Lenses
    listGroupsForUserResponse_isTruncated,
    listGroupsForUserResponse_marker,
    listGroupsForUserResponse_httpStatus,
    listGroupsForUserResponse_groups,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
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
    -- | The name of the user to list groups for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listGroupsForUser_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listGroupsForUser_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'userName', 'listGroupsForUser_userName' - The name of the user to list groups for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListGroupsForUser ::
  -- | 'userName'
  Prelude.Text ->
  ListGroupsForUser
newListGroupsForUser pUserName_ =
  ListGroupsForUser'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      userName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listGroupsForUser_marker :: Lens.Lens' ListGroupsForUser (Prelude.Maybe Prelude.Text)
listGroupsForUser_marker = Lens.lens (\ListGroupsForUser' {marker} -> marker) (\s@ListGroupsForUser' {} a -> s {marker = a} :: ListGroupsForUser)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listGroupsForUser_maxItems :: Lens.Lens' ListGroupsForUser (Prelude.Maybe Prelude.Natural)
listGroupsForUser_maxItems = Lens.lens (\ListGroupsForUser' {maxItems} -> maxItems) (\s@ListGroupsForUser' {} a -> s {maxItems = a} :: ListGroupsForUser)

-- | The name of the user to list groups for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listGroupsForUser_userName :: Lens.Lens' ListGroupsForUser Prelude.Text
listGroupsForUser_userName = Lens.lens (\ListGroupsForUser' {userName} -> userName) (\s@ListGroupsForUser' {} a -> s {userName = a} :: ListGroupsForUser)

instance Core.AWSPager ListGroupsForUser where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupsForUserResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listGroupsForUserResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGroupsForUser_marker
          Lens..~ rs
          Lens.^? listGroupsForUserResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListGroupsForUser where
  type
    AWSResponse ListGroupsForUser =
      ListGroupsForUserResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListGroupsForUserResult"
      ( \s h x ->
          ListGroupsForUserResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "Groups" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListGroupsForUser where
  hashWithSalt _salt ListGroupsForUser' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListGroupsForUser where
  rnf ListGroupsForUser' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ListGroupsForUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListGroupsForUser where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGroupsForUser where
  toQuery ListGroupsForUser' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListGroupsForUser" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "UserName" Data.=: userName
      ]

-- | Contains the response to a successful ListGroupsForUser request.
--
-- /See:/ 'newListGroupsForUserResponse' smart constructor.
data ListGroupsForUserResponse = ListGroupsForUserResponse'
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
-- Create a value of 'ListGroupsForUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listGroupsForUserResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listGroupsForUserResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listGroupsForUserResponse_httpStatus' - The response's http status code.
--
-- 'groups', 'listGroupsForUserResponse_groups' - A list of groups.
newListGroupsForUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupsForUserResponse
newListGroupsForUserResponse pHttpStatus_ =
  ListGroupsForUserResponse'
    { isTruncated =
        Prelude.Nothing,
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
listGroupsForUserResponse_isTruncated :: Lens.Lens' ListGroupsForUserResponse (Prelude.Maybe Prelude.Bool)
listGroupsForUserResponse_isTruncated = Lens.lens (\ListGroupsForUserResponse' {isTruncated} -> isTruncated) (\s@ListGroupsForUserResponse' {} a -> s {isTruncated = a} :: ListGroupsForUserResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listGroupsForUserResponse_marker :: Lens.Lens' ListGroupsForUserResponse (Prelude.Maybe Prelude.Text)
listGroupsForUserResponse_marker = Lens.lens (\ListGroupsForUserResponse' {marker} -> marker) (\s@ListGroupsForUserResponse' {} a -> s {marker = a} :: ListGroupsForUserResponse)

-- | The response's http status code.
listGroupsForUserResponse_httpStatus :: Lens.Lens' ListGroupsForUserResponse Prelude.Int
listGroupsForUserResponse_httpStatus = Lens.lens (\ListGroupsForUserResponse' {httpStatus} -> httpStatus) (\s@ListGroupsForUserResponse' {} a -> s {httpStatus = a} :: ListGroupsForUserResponse)

-- | A list of groups.
listGroupsForUserResponse_groups :: Lens.Lens' ListGroupsForUserResponse [Group]
listGroupsForUserResponse_groups = Lens.lens (\ListGroupsForUserResponse' {groups} -> groups) (\s@ListGroupsForUserResponse' {} a -> s {groups = a} :: ListGroupsForUserResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListGroupsForUserResponse where
  rnf ListGroupsForUserResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf groups
