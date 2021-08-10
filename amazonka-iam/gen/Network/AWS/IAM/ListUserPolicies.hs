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
-- Module      : Network.AWS.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies embedded in the specified IAM
-- user.
--
-- An IAM user can also have managed policies attached to it. To list the
-- managed policies that are attached to a user, use
-- ListAttachedUserPolicies. For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- user, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListUserPolicies
  ( -- * Creating a Request
    ListUserPolicies (..),
    newListUserPolicies,

    -- * Request Lenses
    listUserPolicies_maxItems,
    listUserPolicies_marker,
    listUserPolicies_userName,

    -- * Destructuring the Response
    ListUserPoliciesResponse (..),
    newListUserPoliciesResponse,

    -- * Response Lenses
    listUserPoliciesResponse_isTruncated,
    listUserPoliciesResponse_marker,
    listUserPoliciesResponse_httpStatus,
    listUserPoliciesResponse_policyNames,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListUserPolicies' smart constructor.
data ListUserPolicies = ListUserPolicies'
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
    -- | The name of the user to list policies for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listUserPolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listUserPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'userName', 'listUserPolicies_userName' - The name of the user to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListUserPolicies ::
  -- | 'userName'
  Prelude.Text ->
  ListUserPolicies
newListUserPolicies pUserName_ =
  ListUserPolicies'
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
listUserPolicies_maxItems :: Lens.Lens' ListUserPolicies (Prelude.Maybe Prelude.Natural)
listUserPolicies_maxItems = Lens.lens (\ListUserPolicies' {maxItems} -> maxItems) (\s@ListUserPolicies' {} a -> s {maxItems = a} :: ListUserPolicies)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listUserPolicies_marker :: Lens.Lens' ListUserPolicies (Prelude.Maybe Prelude.Text)
listUserPolicies_marker = Lens.lens (\ListUserPolicies' {marker} -> marker) (\s@ListUserPolicies' {} a -> s {marker = a} :: ListUserPolicies)

-- | The name of the user to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listUserPolicies_userName :: Lens.Lens' ListUserPolicies Prelude.Text
listUserPolicies_userName = Lens.lens (\ListUserPolicies' {userName} -> userName) (\s@ListUserPolicies' {} a -> s {userName = a} :: ListUserPolicies)

instance Core.AWSPager ListUserPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUserPoliciesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listUserPoliciesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUserPolicies_marker
          Lens..~ rs
          Lens.^? listUserPoliciesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListUserPolicies where
  type
    AWSResponse ListUserPolicies =
      ListUserPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListUserPoliciesResult"
      ( \s h x ->
          ListUserPoliciesResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "PolicyNames" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListUserPolicies

instance Prelude.NFData ListUserPolicies

instance Core.ToHeaders ListUserPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListUserPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListUserPolicies where
  toQuery ListUserPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListUserPolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "UserName" Core.=: userName
      ]

-- | Contains the response to a successful ListUserPolicies request.
--
-- /See:/ 'newListUserPoliciesResponse' smart constructor.
data ListUserPoliciesResponse = ListUserPoliciesResponse'
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
    -- | A list of policy names.
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listUserPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listUserPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listUserPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'policyNames', 'listUserPoliciesResponse_policyNames' - A list of policy names.
newListUserPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserPoliciesResponse
newListUserPoliciesResponse pHttpStatus_ =
  ListUserPoliciesResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      policyNames = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listUserPoliciesResponse_isTruncated :: Lens.Lens' ListUserPoliciesResponse (Prelude.Maybe Prelude.Bool)
listUserPoliciesResponse_isTruncated = Lens.lens (\ListUserPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListUserPoliciesResponse' {} a -> s {isTruncated = a} :: ListUserPoliciesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listUserPoliciesResponse_marker :: Lens.Lens' ListUserPoliciesResponse (Prelude.Maybe Prelude.Text)
listUserPoliciesResponse_marker = Lens.lens (\ListUserPoliciesResponse' {marker} -> marker) (\s@ListUserPoliciesResponse' {} a -> s {marker = a} :: ListUserPoliciesResponse)

-- | The response's http status code.
listUserPoliciesResponse_httpStatus :: Lens.Lens' ListUserPoliciesResponse Prelude.Int
listUserPoliciesResponse_httpStatus = Lens.lens (\ListUserPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListUserPoliciesResponse' {} a -> s {httpStatus = a} :: ListUserPoliciesResponse)

-- | A list of policy names.
listUserPoliciesResponse_policyNames :: Lens.Lens' ListUserPoliciesResponse [Prelude.Text]
listUserPoliciesResponse_policyNames = Lens.lens (\ListUserPoliciesResponse' {policyNames} -> policyNames) (\s@ListUserPoliciesResponse' {} a -> s {policyNames = a} :: ListUserPoliciesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListUserPoliciesResponse
