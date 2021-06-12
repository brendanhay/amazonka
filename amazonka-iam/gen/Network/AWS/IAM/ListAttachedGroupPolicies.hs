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
-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM group.
--
-- An IAM group can also have inline policies embedded with it. To list the
-- inline policies for a group, use ListGroupPolicies. For information
-- about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified group (or none that match the
-- specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedGroupPolicies
  ( -- * Creating a Request
    ListAttachedGroupPolicies (..),
    newListAttachedGroupPolicies,

    -- * Request Lenses
    listAttachedGroupPolicies_pathPrefix,
    listAttachedGroupPolicies_maxItems,
    listAttachedGroupPolicies_marker,
    listAttachedGroupPolicies_groupName,

    -- * Destructuring the Response
    ListAttachedGroupPoliciesResponse (..),
    newListAttachedGroupPoliciesResponse,

    -- * Response Lenses
    listAttachedGroupPoliciesResponse_isTruncated,
    listAttachedGroupPoliciesResponse_attachedPolicies,
    listAttachedGroupPoliciesResponse_marker,
    listAttachedGroupPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAttachedGroupPolicies' smart constructor.
data ListAttachedGroupPolicies = ListAttachedGroupPolicies'
  { -- | The path prefix for filtering the results. This parameter is optional.
    -- If it is not included, it defaults to a slash (\/), listing all
    -- policies.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    pathPrefix :: Core.Maybe Core.Text,
    -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Core.Maybe Core.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Core.Maybe Core.Text,
    -- | The name (friendly name, not ARN) of the group to list attached policies
    -- for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedGroupPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefix', 'listAttachedGroupPolicies_pathPrefix' - The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'maxItems', 'listAttachedGroupPolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listAttachedGroupPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'groupName', 'listAttachedGroupPolicies_groupName' - The name (friendly name, not ARN) of the group to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListAttachedGroupPolicies ::
  -- | 'groupName'
  Core.Text ->
  ListAttachedGroupPolicies
newListAttachedGroupPolicies pGroupName_ =
  ListAttachedGroupPolicies'
    { pathPrefix =
        Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing,
      groupName = pGroupName_
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
listAttachedGroupPolicies_pathPrefix :: Lens.Lens' ListAttachedGroupPolicies (Core.Maybe Core.Text)
listAttachedGroupPolicies_pathPrefix = Lens.lens (\ListAttachedGroupPolicies' {pathPrefix} -> pathPrefix) (\s@ListAttachedGroupPolicies' {} a -> s {pathPrefix = a} :: ListAttachedGroupPolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAttachedGroupPolicies_maxItems :: Lens.Lens' ListAttachedGroupPolicies (Core.Maybe Core.Natural)
listAttachedGroupPolicies_maxItems = Lens.lens (\ListAttachedGroupPolicies' {maxItems} -> maxItems) (\s@ListAttachedGroupPolicies' {} a -> s {maxItems = a} :: ListAttachedGroupPolicies)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAttachedGroupPolicies_marker :: Lens.Lens' ListAttachedGroupPolicies (Core.Maybe Core.Text)
listAttachedGroupPolicies_marker = Lens.lens (\ListAttachedGroupPolicies' {marker} -> marker) (\s@ListAttachedGroupPolicies' {} a -> s {marker = a} :: ListAttachedGroupPolicies)

-- | The name (friendly name, not ARN) of the group to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAttachedGroupPolicies_groupName :: Lens.Lens' ListAttachedGroupPolicies Core.Text
listAttachedGroupPolicies_groupName = Lens.lens (\ListAttachedGroupPolicies' {groupName} -> groupName) (\s@ListAttachedGroupPolicies' {} a -> s {groupName = a} :: ListAttachedGroupPolicies)

instance Core.AWSPager ListAttachedGroupPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedGroupPoliciesResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listAttachedGroupPoliciesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAttachedGroupPolicies_marker
          Lens..~ rs
          Lens.^? listAttachedGroupPoliciesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListAttachedGroupPolicies where
  type
    AWSResponse ListAttachedGroupPolicies =
      ListAttachedGroupPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAttachedGroupPoliciesResult"
      ( \s h x ->
          ListAttachedGroupPoliciesResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> ( x Core..@? "AttachedPolicies" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAttachedGroupPolicies

instance Core.NFData ListAttachedGroupPolicies

instance Core.ToHeaders ListAttachedGroupPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListAttachedGroupPolicies where
  toPath = Core.const "/"

instance Core.ToQuery ListAttachedGroupPolicies where
  toQuery ListAttachedGroupPolicies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListAttachedGroupPolicies" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "PathPrefix" Core.=: pathPrefix,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "GroupName" Core.=: groupName
      ]

-- | Contains the response to a successful ListAttachedGroupPolicies request.
--
-- /See:/ 'newListAttachedGroupPoliciesResponse' smart constructor.
data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse'
  { -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | A list of the attached policies.
    attachedPolicies :: Core.Maybe [AttachedPolicy],
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedGroupPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listAttachedGroupPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'attachedPolicies', 'listAttachedGroupPoliciesResponse_attachedPolicies' - A list of the attached policies.
--
-- 'marker', 'listAttachedGroupPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAttachedGroupPoliciesResponse_httpStatus' - The response's http status code.
newListAttachedGroupPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAttachedGroupPoliciesResponse
newListAttachedGroupPoliciesResponse pHttpStatus_ =
  ListAttachedGroupPoliciesResponse'
    { isTruncated =
        Core.Nothing,
      attachedPolicies = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAttachedGroupPoliciesResponse_isTruncated :: Lens.Lens' ListAttachedGroupPoliciesResponse (Core.Maybe Core.Bool)
listAttachedGroupPoliciesResponse_isTruncated = Lens.lens (\ListAttachedGroupPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListAttachedGroupPoliciesResponse' {} a -> s {isTruncated = a} :: ListAttachedGroupPoliciesResponse)

-- | A list of the attached policies.
listAttachedGroupPoliciesResponse_attachedPolicies :: Lens.Lens' ListAttachedGroupPoliciesResponse (Core.Maybe [AttachedPolicy])
listAttachedGroupPoliciesResponse_attachedPolicies = Lens.lens (\ListAttachedGroupPoliciesResponse' {attachedPolicies} -> attachedPolicies) (\s@ListAttachedGroupPoliciesResponse' {} a -> s {attachedPolicies = a} :: ListAttachedGroupPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAttachedGroupPoliciesResponse_marker :: Lens.Lens' ListAttachedGroupPoliciesResponse (Core.Maybe Core.Text)
listAttachedGroupPoliciesResponse_marker = Lens.lens (\ListAttachedGroupPoliciesResponse' {marker} -> marker) (\s@ListAttachedGroupPoliciesResponse' {} a -> s {marker = a} :: ListAttachedGroupPoliciesResponse)

-- | The response's http status code.
listAttachedGroupPoliciesResponse_httpStatus :: Lens.Lens' ListAttachedGroupPoliciesResponse Core.Int
listAttachedGroupPoliciesResponse_httpStatus = Lens.lens (\ListAttachedGroupPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedGroupPoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedGroupPoliciesResponse)

instance
  Core.NFData
    ListAttachedGroupPoliciesResponse
