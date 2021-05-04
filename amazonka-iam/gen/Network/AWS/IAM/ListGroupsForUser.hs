{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.ListGroupsForUser
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IAM.ListGroupsForUser
  ( -- * Creating a Request
    ListGroupsForUser (..),
    newListGroupsForUser,

    -- * Request Lenses
    listGroupsForUser_maxItems,
    listGroupsForUser_marker,
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupsForUser' smart constructor.
data ListGroupsForUser = ListGroupsForUser'
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
    marker :: Prelude.Maybe Prelude.Text,
    -- | The name of the user to list groups for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListGroupsForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'marker', 'listGroupsForUser_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      userName = pUserName_
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
listGroupsForUser_maxItems :: Lens.Lens' ListGroupsForUser (Prelude.Maybe Prelude.Natural)
listGroupsForUser_maxItems = Lens.lens (\ListGroupsForUser' {maxItems} -> maxItems) (\s@ListGroupsForUser' {} a -> s {maxItems = a} :: ListGroupsForUser)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listGroupsForUser_marker :: Lens.Lens' ListGroupsForUser (Prelude.Maybe Prelude.Text)
listGroupsForUser_marker = Lens.lens (\ListGroupsForUser' {marker} -> marker) (\s@ListGroupsForUser' {} a -> s {marker = a} :: ListGroupsForUser)

-- | The name of the user to list groups for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listGroupsForUser_userName :: Lens.Lens' ListGroupsForUser Prelude.Text
listGroupsForUser_userName = Lens.lens (\ListGroupsForUser' {userName} -> userName) (\s@ListGroupsForUser' {} a -> s {userName = a} :: ListGroupsForUser)

instance Pager.AWSPager ListGroupsForUser where
  page rq rs
    | Pager.stop
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
          Lens.& listGroupsForUser_marker
          Lens..~ rs
          Lens.^? listGroupsForUserResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest ListGroupsForUser where
  type Rs ListGroupsForUser = ListGroupsForUserResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListGroupsForUserResult"
      ( \s h x ->
          ListGroupsForUserResponse'
            Prelude.<$> (x Prelude..@? "IsTruncated")
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "Groups" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListGroupsForUser

instance Prelude.NFData ListGroupsForUser

instance Prelude.ToHeaders ListGroupsForUser where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListGroupsForUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListGroupsForUser where
  toQuery ListGroupsForUser' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ListGroupsForUser" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker,
        "UserName" Prelude.=: userName
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listGroupsForUserResponse_groups = Lens.lens (\ListGroupsForUserResponse' {groups} -> groups) (\s@ListGroupsForUserResponse' {} a -> s {groups = a} :: ListGroupsForUserResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListGroupsForUserResponse
