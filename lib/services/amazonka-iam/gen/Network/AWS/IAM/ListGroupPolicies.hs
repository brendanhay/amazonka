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
-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the
-- specified IAM group.
--
-- An IAM group can also have managed policies attached to it. To list the
-- managed policies that are attached to a group, use
-- ListAttachedGroupPolicies. For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- group, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupPolicies
  ( -- * Creating a Request
    ListGroupPolicies (..),
    newListGroupPolicies,

    -- * Request Lenses
    listGroupPolicies_maxItems,
    listGroupPolicies_marker,
    listGroupPolicies_groupName,

    -- * Destructuring the Response
    ListGroupPoliciesResponse (..),
    newListGroupPoliciesResponse,

    -- * Response Lenses
    listGroupPoliciesResponse_isTruncated,
    listGroupPoliciesResponse_marker,
    listGroupPoliciesResponse_httpStatus,
    listGroupPoliciesResponse_policyNames,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupPolicies' smart constructor.
data ListGroupPolicies = ListGroupPolicies'
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
    -- | The name of the group to list policies for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxItems', 'listGroupPolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listGroupPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'groupName', 'listGroupPolicies_groupName' - The name of the group to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListGroupPolicies ::
  -- | 'groupName'
  Prelude.Text ->
  ListGroupPolicies
newListGroupPolicies pGroupName_ =
  ListGroupPolicies'
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
listGroupPolicies_maxItems :: Lens.Lens' ListGroupPolicies (Prelude.Maybe Prelude.Natural)
listGroupPolicies_maxItems = Lens.lens (\ListGroupPolicies' {maxItems} -> maxItems) (\s@ListGroupPolicies' {} a -> s {maxItems = a} :: ListGroupPolicies)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listGroupPolicies_marker :: Lens.Lens' ListGroupPolicies (Prelude.Maybe Prelude.Text)
listGroupPolicies_marker = Lens.lens (\ListGroupPolicies' {marker} -> marker) (\s@ListGroupPolicies' {} a -> s {marker = a} :: ListGroupPolicies)

-- | The name of the group to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listGroupPolicies_groupName :: Lens.Lens' ListGroupPolicies Prelude.Text
listGroupPolicies_groupName = Lens.lens (\ListGroupPolicies' {groupName} -> groupName) (\s@ListGroupPolicies' {} a -> s {groupName = a} :: ListGroupPolicies)

instance Core.AWSPager ListGroupPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGroupPoliciesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listGroupPoliciesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listGroupPolicies_marker
          Lens..~ rs
          Lens.^? listGroupPoliciesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListGroupPolicies where
  type
    AWSResponse ListGroupPolicies =
      ListGroupPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListGroupPoliciesResult"
      ( \s h x ->
          ListGroupPoliciesResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "PolicyNames" Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListGroupPolicies

instance Prelude.NFData ListGroupPolicies

instance Core.ToHeaders ListGroupPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListGroupPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListGroupPolicies where
  toQuery ListGroupPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListGroupPolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "GroupName" Core.=: groupName
      ]

-- | Contains the response to a successful ListGroupPolicies request.
--
-- /See:/ 'newListGroupPoliciesResponse' smart constructor.
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'
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
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listGroupPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listGroupPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listGroupPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'policyNames', 'listGroupPoliciesResponse_policyNames' - A list of policy names.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListGroupPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGroupPoliciesResponse
newListGroupPoliciesResponse pHttpStatus_ =
  ListGroupPoliciesResponse'
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
listGroupPoliciesResponse_isTruncated :: Lens.Lens' ListGroupPoliciesResponse (Prelude.Maybe Prelude.Bool)
listGroupPoliciesResponse_isTruncated = Lens.lens (\ListGroupPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListGroupPoliciesResponse' {} a -> s {isTruncated = a} :: ListGroupPoliciesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listGroupPoliciesResponse_marker :: Lens.Lens' ListGroupPoliciesResponse (Prelude.Maybe Prelude.Text)
listGroupPoliciesResponse_marker = Lens.lens (\ListGroupPoliciesResponse' {marker} -> marker) (\s@ListGroupPoliciesResponse' {} a -> s {marker = a} :: ListGroupPoliciesResponse)

-- | The response's http status code.
listGroupPoliciesResponse_httpStatus :: Lens.Lens' ListGroupPoliciesResponse Prelude.Int
listGroupPoliciesResponse_httpStatus = Lens.lens (\ListGroupPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListGroupPoliciesResponse' {} a -> s {httpStatus = a} :: ListGroupPoliciesResponse)

-- | A list of policy names.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listGroupPoliciesResponse_policyNames :: Lens.Lens' ListGroupPoliciesResponse [Prelude.Text]
listGroupPoliciesResponse_policyNames = Lens.lens (\ListGroupPoliciesResponse' {policyNames} -> policyNames) (\s@ListGroupPoliciesResponse' {} a -> s {policyNames = a} :: ListGroupPoliciesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListGroupPoliciesResponse
