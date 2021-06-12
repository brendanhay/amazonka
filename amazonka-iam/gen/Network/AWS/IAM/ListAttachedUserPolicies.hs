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
-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM user.
--
-- An IAM user can also have inline policies embedded with it. To list the
-- inline policies for a user, use ListUserPolicies. For information about
-- policies, see
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
module Network.AWS.IAM.ListAttachedUserPolicies
  ( -- * Creating a Request
    ListAttachedUserPolicies (..),
    newListAttachedUserPolicies,

    -- * Request Lenses
    listAttachedUserPolicies_pathPrefix,
    listAttachedUserPolicies_maxItems,
    listAttachedUserPolicies_marker,
    listAttachedUserPolicies_userName,

    -- * Destructuring the Response
    ListAttachedUserPoliciesResponse (..),
    newListAttachedUserPoliciesResponse,

    -- * Response Lenses
    listAttachedUserPoliciesResponse_isTruncated,
    listAttachedUserPoliciesResponse_attachedPolicies,
    listAttachedUserPoliciesResponse_marker,
    listAttachedUserPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAttachedUserPolicies' smart constructor.
data ListAttachedUserPolicies = ListAttachedUserPolicies'
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
    -- | The name (friendly name, not ARN) of the user to list attached policies
    -- for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAttachedUserPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefix', 'listAttachedUserPolicies_pathPrefix' - The path prefix for filtering the results. This parameter is optional.
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
-- 'maxItems', 'listAttachedUserPolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listAttachedUserPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'userName', 'listAttachedUserPolicies_userName' - The name (friendly name, not ARN) of the user to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListAttachedUserPolicies ::
  -- | 'userName'
  Core.Text ->
  ListAttachedUserPolicies
newListAttachedUserPolicies pUserName_ =
  ListAttachedUserPolicies'
    { pathPrefix =
        Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing,
      userName = pUserName_
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
listAttachedUserPolicies_pathPrefix :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Core.Text)
listAttachedUserPolicies_pathPrefix = Lens.lens (\ListAttachedUserPolicies' {pathPrefix} -> pathPrefix) (\s@ListAttachedUserPolicies' {} a -> s {pathPrefix = a} :: ListAttachedUserPolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAttachedUserPolicies_maxItems :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Core.Natural)
listAttachedUserPolicies_maxItems = Lens.lens (\ListAttachedUserPolicies' {maxItems} -> maxItems) (\s@ListAttachedUserPolicies' {} a -> s {maxItems = a} :: ListAttachedUserPolicies)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAttachedUserPolicies_marker :: Lens.Lens' ListAttachedUserPolicies (Core.Maybe Core.Text)
listAttachedUserPolicies_marker = Lens.lens (\ListAttachedUserPolicies' {marker} -> marker) (\s@ListAttachedUserPolicies' {} a -> s {marker = a} :: ListAttachedUserPolicies)

-- | The name (friendly name, not ARN) of the user to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAttachedUserPolicies_userName :: Lens.Lens' ListAttachedUserPolicies Core.Text
listAttachedUserPolicies_userName = Lens.lens (\ListAttachedUserPolicies' {userName} -> userName) (\s@ListAttachedUserPolicies' {} a -> s {userName = a} :: ListAttachedUserPolicies)

instance Core.AWSPager ListAttachedUserPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedUserPoliciesResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listAttachedUserPoliciesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAttachedUserPolicies_marker
          Lens..~ rs
          Lens.^? listAttachedUserPoliciesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListAttachedUserPolicies where
  type
    AWSResponse ListAttachedUserPolicies =
      ListAttachedUserPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListAttachedUserPoliciesResult"
      ( \s h x ->
          ListAttachedUserPoliciesResponse'
            Core.<$> (x Core..@? "IsTruncated")
            Core.<*> ( x Core..@? "AttachedPolicies" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAttachedUserPolicies

instance Core.NFData ListAttachedUserPolicies

instance Core.ToHeaders ListAttachedUserPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListAttachedUserPolicies where
  toPath = Core.const "/"

instance Core.ToQuery ListAttachedUserPolicies where
  toQuery ListAttachedUserPolicies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListAttachedUserPolicies" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "PathPrefix" Core.=: pathPrefix,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "UserName" Core.=: userName
      ]

-- | Contains the response to a successful ListAttachedUserPolicies request.
--
-- /See:/ 'newListAttachedUserPoliciesResponse' smart constructor.
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
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
-- Create a value of 'ListAttachedUserPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listAttachedUserPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'attachedPolicies', 'listAttachedUserPoliciesResponse_attachedPolicies' - A list of the attached policies.
--
-- 'marker', 'listAttachedUserPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAttachedUserPoliciesResponse_httpStatus' - The response's http status code.
newListAttachedUserPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAttachedUserPoliciesResponse
newListAttachedUserPoliciesResponse pHttpStatus_ =
  ListAttachedUserPoliciesResponse'
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
listAttachedUserPoliciesResponse_isTruncated :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe Core.Bool)
listAttachedUserPoliciesResponse_isTruncated = Lens.lens (\ListAttachedUserPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListAttachedUserPoliciesResponse' {} a -> s {isTruncated = a} :: ListAttachedUserPoliciesResponse)

-- | A list of the attached policies.
listAttachedUserPoliciesResponse_attachedPolicies :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe [AttachedPolicy])
listAttachedUserPoliciesResponse_attachedPolicies = Lens.lens (\ListAttachedUserPoliciesResponse' {attachedPolicies} -> attachedPolicies) (\s@ListAttachedUserPoliciesResponse' {} a -> s {attachedPolicies = a} :: ListAttachedUserPoliciesResponse) Core.. Lens.mapping Lens._Coerce

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAttachedUserPoliciesResponse_marker :: Lens.Lens' ListAttachedUserPoliciesResponse (Core.Maybe Core.Text)
listAttachedUserPoliciesResponse_marker = Lens.lens (\ListAttachedUserPoliciesResponse' {marker} -> marker) (\s@ListAttachedUserPoliciesResponse' {} a -> s {marker = a} :: ListAttachedUserPoliciesResponse)

-- | The response's http status code.
listAttachedUserPoliciesResponse_httpStatus :: Lens.Lens' ListAttachedUserPoliciesResponse Core.Int
listAttachedUserPoliciesResponse_httpStatus = Lens.lens (\ListAttachedUserPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedUserPoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedUserPoliciesResponse)

instance Core.NFData ListAttachedUserPoliciesResponse
