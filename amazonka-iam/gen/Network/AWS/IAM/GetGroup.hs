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
-- Module      : Network.AWS.IAM.GetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of IAM users that are in the specified IAM group. You can
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetGroup
  ( -- * Creating a Request
    GetGroup (..),
    newGetGroup,

    -- * Request Lenses
    getGroup_maxItems,
    getGroup_marker,
    getGroup_groupName,

    -- * Destructuring the Response
    GetGroupResponse (..),
    newGetGroupResponse,

    -- * Response Lenses
    getGroupResponse_isTruncated,
    getGroupResponse_marker,
    getGroupResponse_httpStatus,
    getGroupResponse_group,
    getGroupResponse_users,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroup' smart constructor.
data GetGroup = GetGroup'
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
    -- | The name of the group.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'getGroup_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'getGroup_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'groupName', 'getGroup_groupName' - The name of the group.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newGetGroup ::
  -- | 'groupName'
  Prelude.Text ->
  GetGroup
newGetGroup pGroupName_ =
  GetGroup'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      groupName = pGroupName_
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
getGroup_maxItems :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Natural)
getGroup_maxItems = Lens.lens (\GetGroup' {maxItems} -> maxItems) (\s@GetGroup' {} a -> s {maxItems = a} :: GetGroup)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
getGroup_marker :: Lens.Lens' GetGroup (Prelude.Maybe Prelude.Text)
getGroup_marker = Lens.lens (\GetGroup' {marker} -> marker) (\s@GetGroup' {} a -> s {marker = a} :: GetGroup)

-- | The name of the group.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
getGroup_groupName :: Lens.Lens' GetGroup Prelude.Text
getGroup_groupName = Lens.lens (\GetGroup' {groupName} -> groupName) (\s@GetGroup' {} a -> s {groupName = a} :: GetGroup)

instance Pager.AWSPager GetGroup where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getGroupResponse_isTruncated Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? getGroupResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getGroup_marker
          Lens..~ rs
          Lens.^? getGroupResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest GetGroup where
  type Rs GetGroup = GetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetGroupResult"
      ( \s h x ->
          GetGroupResponse'
            Prelude.<$> (x Prelude..@? "IsTruncated")
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "Group")
            Prelude.<*> ( x Prelude..@? "Users" Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable GetGroup

instance Prelude.NFData GetGroup

instance Prelude.ToHeaders GetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetGroup where
  toQuery GetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Prelude.=: maxItems,
        "Marker" Prelude.=: marker,
        "GroupName" Prelude.=: groupName
      ]

-- | Contains the response to a successful GetGroup request.
--
-- /See:/ 'newGetGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
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
    -- | A structure that contains details about the group.
    group' :: Group,
    -- | A list of users in the group.
    users :: [User]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'getGroupResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'getGroupResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'getGroupResponse_httpStatus' - The response's http status code.
--
-- 'group'', 'getGroupResponse_group' - A structure that contains details about the group.
--
-- 'users', 'getGroupResponse_users' - A list of users in the group.
newGetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'group''
  Group ->
  GetGroupResponse
newGetGroupResponse pHttpStatus_ pGroup_ =
  GetGroupResponse'
    { isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      group' = pGroup_,
      users = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
getGroupResponse_isTruncated :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Bool)
getGroupResponse_isTruncated = Lens.lens (\GetGroupResponse' {isTruncated} -> isTruncated) (\s@GetGroupResponse' {} a -> s {isTruncated = a} :: GetGroupResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
getGroupResponse_marker :: Lens.Lens' GetGroupResponse (Prelude.Maybe Prelude.Text)
getGroupResponse_marker = Lens.lens (\GetGroupResponse' {marker} -> marker) (\s@GetGroupResponse' {} a -> s {marker = a} :: GetGroupResponse)

-- | The response's http status code.
getGroupResponse_httpStatus :: Lens.Lens' GetGroupResponse Prelude.Int
getGroupResponse_httpStatus = Lens.lens (\GetGroupResponse' {httpStatus} -> httpStatus) (\s@GetGroupResponse' {} a -> s {httpStatus = a} :: GetGroupResponse)

-- | A structure that contains details about the group.
getGroupResponse_group :: Lens.Lens' GetGroupResponse Group
getGroupResponse_group = Lens.lens (\GetGroupResponse' {group'} -> group') (\s@GetGroupResponse' {} a -> s {group' = a} :: GetGroupResponse)

-- | A list of users in the group.
getGroupResponse_users :: Lens.Lens' GetGroupResponse [User]
getGroupResponse_users = Lens.lens (\GetGroupResponse' {users} -> users) (\s@GetGroupResponse' {} a -> s {users = a} :: GetGroupResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetGroupResponse
