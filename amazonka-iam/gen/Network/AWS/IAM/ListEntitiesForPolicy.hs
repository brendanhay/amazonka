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
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all IAM users, groups, and roles that the specified managed policy
-- is attached to.
--
-- You can use the optional @EntityFilter@ parameter to limit the results
-- to a particular type of entity (users, groups, or roles). For example,
-- to list only the roles that are attached to the specified policy, set
-- @EntityFilter@ to @Role@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
  ( -- * Creating a Request
    ListEntitiesForPolicy (..),
    newListEntitiesForPolicy,

    -- * Request Lenses
    listEntitiesForPolicy_entityFilter,
    listEntitiesForPolicy_policyUsageFilter,
    listEntitiesForPolicy_pathPrefix,
    listEntitiesForPolicy_maxItems,
    listEntitiesForPolicy_marker,
    listEntitiesForPolicy_policyArn,

    -- * Destructuring the Response
    ListEntitiesForPolicyResponse (..),
    newListEntitiesForPolicyResponse,

    -- * Response Lenses
    listEntitiesForPolicyResponse_policyRoles,
    listEntitiesForPolicyResponse_isTruncated,
    listEntitiesForPolicyResponse_policyUsers,
    listEntitiesForPolicyResponse_policyGroups,
    listEntitiesForPolicyResponse_marker,
    listEntitiesForPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { -- | The entity type to use for filtering the results.
    --
    -- For example, when @EntityFilter@ is @Role@, only the roles that are
    -- attached to the specified policy are returned. This parameter is
    -- optional. If it is not included, all attached entities (users, groups,
    -- and roles) are returned. The argument for this parameter must be one of
    -- the valid values listed below.
    entityFilter :: Core.Maybe EntityType,
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies,
    -- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
    -- policies used to set permissions boundaries, set the value
    -- to @PermissionsBoundary@.
    --
    -- This parameter is optional. If it is not included, all policies are
    -- returned.
    policyUsageFilter :: Core.Maybe PolicyUsageType,
    -- | The path prefix for filtering the results. This parameter is optional.
    -- If it is not included, it defaults to a slash (\/), listing all
    -- entities.
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
    -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
    -- versions.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    policyArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEntitiesForPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityFilter', 'listEntitiesForPolicy_entityFilter' - The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@, only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned. The argument for this parameter must be one of
-- the valid values listed below.
--
-- 'policyUsageFilter', 'listEntitiesForPolicy_policyUsageFilter' - The policy usage method to use for filtering the results.
--
-- To list only permissions policies,
-- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
-- policies used to set permissions boundaries, set the value
-- to @PermissionsBoundary@.
--
-- This parameter is optional. If it is not included, all policies are
-- returned.
--
-- 'pathPrefix', 'listEntitiesForPolicy_pathPrefix' - The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- entities.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'maxItems', 'listEntitiesForPolicy_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listEntitiesForPolicy_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'policyArn', 'listEntitiesForPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListEntitiesForPolicy ::
  -- | 'policyArn'
  Core.Text ->
  ListEntitiesForPolicy
newListEntitiesForPolicy pPolicyArn_ =
  ListEntitiesForPolicy'
    { entityFilter = Core.Nothing,
      policyUsageFilter = Core.Nothing,
      pathPrefix = Core.Nothing,
      maxItems = Core.Nothing,
      marker = Core.Nothing,
      policyArn = pPolicyArn_
    }

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@, only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned. The argument for this parameter must be one of
-- the valid values listed below.
listEntitiesForPolicy_entityFilter :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe EntityType)
listEntitiesForPolicy_entityFilter = Lens.lens (\ListEntitiesForPolicy' {entityFilter} -> entityFilter) (\s@ListEntitiesForPolicy' {} a -> s {entityFilter = a} :: ListEntitiesForPolicy)

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies,
-- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
-- policies used to set permissions boundaries, set the value
-- to @PermissionsBoundary@.
--
-- This parameter is optional. If it is not included, all policies are
-- returned.
listEntitiesForPolicy_policyUsageFilter :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe PolicyUsageType)
listEntitiesForPolicy_policyUsageFilter = Lens.lens (\ListEntitiesForPolicy' {policyUsageFilter} -> policyUsageFilter) (\s@ListEntitiesForPolicy' {} a -> s {policyUsageFilter = a} :: ListEntitiesForPolicy)

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- entities.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
listEntitiesForPolicy_pathPrefix :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Core.Text)
listEntitiesForPolicy_pathPrefix = Lens.lens (\ListEntitiesForPolicy' {pathPrefix} -> pathPrefix) (\s@ListEntitiesForPolicy' {} a -> s {pathPrefix = a} :: ListEntitiesForPolicy)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listEntitiesForPolicy_maxItems :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Core.Natural)
listEntitiesForPolicy_maxItems = Lens.lens (\ListEntitiesForPolicy' {maxItems} -> maxItems) (\s@ListEntitiesForPolicy' {} a -> s {maxItems = a} :: ListEntitiesForPolicy)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listEntitiesForPolicy_marker :: Lens.Lens' ListEntitiesForPolicy (Core.Maybe Core.Text)
listEntitiesForPolicy_marker = Lens.lens (\ListEntitiesForPolicy' {marker} -> marker) (\s@ListEntitiesForPolicy' {} a -> s {marker = a} :: ListEntitiesForPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listEntitiesForPolicy_policyArn :: Lens.Lens' ListEntitiesForPolicy Core.Text
listEntitiesForPolicy_policyArn = Lens.lens (\ListEntitiesForPolicy' {policyArn} -> policyArn) (\s@ListEntitiesForPolicy' {} a -> s {policyArn = a} :: ListEntitiesForPolicy)

instance Core.AWSPager ListEntitiesForPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEntitiesForPolicyResponse_isTruncated
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.isNothing
        ( rs
            Lens.^? listEntitiesForPolicyResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listEntitiesForPolicy_marker
          Lens..~ rs
          Lens.^? listEntitiesForPolicyResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest ListEntitiesForPolicy where
  type
    AWSResponse ListEntitiesForPolicy =
      ListEntitiesForPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListEntitiesForPolicyResult"
      ( \s h x ->
          ListEntitiesForPolicyResponse'
            Core.<$> ( x Core..@? "PolicyRoles" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "IsTruncated")
            Core.<*> ( x Core..@? "PolicyUsers" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "PolicyGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListEntitiesForPolicy

instance Core.NFData ListEntitiesForPolicy

instance Core.ToHeaders ListEntitiesForPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListEntitiesForPolicy where
  toPath = Core.const "/"

instance Core.ToQuery ListEntitiesForPolicy where
  toQuery ListEntitiesForPolicy' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListEntitiesForPolicy" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "EntityFilter" Core.=: entityFilter,
        "PolicyUsageFilter" Core.=: policyUsageFilter,
        "PathPrefix" Core.=: pathPrefix,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "PolicyArn" Core.=: policyArn
      ]

-- | Contains the response to a successful ListEntitiesForPolicy request.
--
-- /See:/ 'newListEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
  { -- | A list of IAM roles that the policy is attached to.
    policyRoles :: Core.Maybe [PolicyRole],
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Core.Maybe Core.Bool,
    -- | A list of IAM users that the policy is attached to.
    policyUsers :: Core.Maybe [PolicyUser],
    -- | A list of IAM groups that the policy is attached to.
    policyGroups :: Core.Maybe [PolicyGroup],
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEntitiesForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRoles', 'listEntitiesForPolicyResponse_policyRoles' - A list of IAM roles that the policy is attached to.
--
-- 'isTruncated', 'listEntitiesForPolicyResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'policyUsers', 'listEntitiesForPolicyResponse_policyUsers' - A list of IAM users that the policy is attached to.
--
-- 'policyGroups', 'listEntitiesForPolicyResponse_policyGroups' - A list of IAM groups that the policy is attached to.
--
-- 'marker', 'listEntitiesForPolicyResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listEntitiesForPolicyResponse_httpStatus' - The response's http status code.
newListEntitiesForPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEntitiesForPolicyResponse
newListEntitiesForPolicyResponse pHttpStatus_ =
  ListEntitiesForPolicyResponse'
    { policyRoles =
        Core.Nothing,
      isTruncated = Core.Nothing,
      policyUsers = Core.Nothing,
      policyGroups = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of IAM roles that the policy is attached to.
listEntitiesForPolicyResponse_policyRoles :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [PolicyRole])
listEntitiesForPolicyResponse_policyRoles = Lens.lens (\ListEntitiesForPolicyResponse' {policyRoles} -> policyRoles) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyRoles = a} :: ListEntitiesForPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listEntitiesForPolicyResponse_isTruncated :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe Core.Bool)
listEntitiesForPolicyResponse_isTruncated = Lens.lens (\ListEntitiesForPolicyResponse' {isTruncated} -> isTruncated) (\s@ListEntitiesForPolicyResponse' {} a -> s {isTruncated = a} :: ListEntitiesForPolicyResponse)

-- | A list of IAM users that the policy is attached to.
listEntitiesForPolicyResponse_policyUsers :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [PolicyUser])
listEntitiesForPolicyResponse_policyUsers = Lens.lens (\ListEntitiesForPolicyResponse' {policyUsers} -> policyUsers) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyUsers = a} :: ListEntitiesForPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of IAM groups that the policy is attached to.
listEntitiesForPolicyResponse_policyGroups :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe [PolicyGroup])
listEntitiesForPolicyResponse_policyGroups = Lens.lens (\ListEntitiesForPolicyResponse' {policyGroups} -> policyGroups) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyGroups = a} :: ListEntitiesForPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listEntitiesForPolicyResponse_marker :: Lens.Lens' ListEntitiesForPolicyResponse (Core.Maybe Core.Text)
listEntitiesForPolicyResponse_marker = Lens.lens (\ListEntitiesForPolicyResponse' {marker} -> marker) (\s@ListEntitiesForPolicyResponse' {} a -> s {marker = a} :: ListEntitiesForPolicyResponse)

-- | The response's http status code.
listEntitiesForPolicyResponse_httpStatus :: Lens.Lens' ListEntitiesForPolicyResponse Core.Int
listEntitiesForPolicyResponse_httpStatus = Lens.lens (\ListEntitiesForPolicyResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesForPolicyResponse' {} a -> s {httpStatus = a} :: ListEntitiesForPolicyResponse)

instance Core.NFData ListEntitiesForPolicyResponse
