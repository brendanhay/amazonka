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
-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed policies that are available in your Amazon Web
-- Services account, including your own customer-defined managed policies
-- and all Amazon Web Services managed policies.
--
-- You can filter the list of policies that is returned using the optional
-- @OnlyAttached@, @Scope@, and @PathPrefix@ parameters. For example, to
-- list only the customer managed policies in your Amazon Web Services
-- account, set @Scope@ to @Local@. To list only Amazon Web Services
-- managed policies, set @Scope@ to @AWS@.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- For more information about managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for a customer manged policy, see GetPolicy.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicies
  ( -- * Creating a Request
    ListPolicies (..),
    newListPolicies,

    -- * Request Lenses
    listPolicies_pathPrefix,
    listPolicies_onlyAttached,
    listPolicies_marker,
    listPolicies_scope,
    listPolicies_maxItems,
    listPolicies_policyUsageFilter,

    -- * Destructuring the Response
    ListPoliciesResponse (..),
    newListPoliciesResponse,

    -- * Response Lenses
    listPoliciesResponse_marker,
    listPoliciesResponse_isTruncated,
    listPoliciesResponse_policies,
    listPoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The path prefix for filtering the results. This parameter is optional.
    -- If it is not included, it defaults to a slash (\/), listing all
    -- policies. This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    pathPrefix :: Prelude.Maybe Prelude.Text,
    -- | A flag to filter the results to only the attached policies.
    --
    -- When @OnlyAttached@ is @true@, the returned list contains only the
    -- policies that are attached to an IAM user, group, or role. When
    -- @OnlyAttached@ is @false@, or when the parameter is not included, all
    -- policies are returned.
    onlyAttached :: Prelude.Maybe Prelude.Bool,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The scope to use for filtering the results.
    --
    -- To list only Amazon Web Services managed policies, set @Scope@ to @AWS@.
    -- To list only the customer managed policies in your Amazon Web Services
    -- account, set @Scope@ to @Local@.
    --
    -- This parameter is optional. If it is not included, or if it is set to
    -- @All@, all policies are returned.
    scope :: Prelude.Maybe PolicyScopeType,
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
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies,
    -- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
    -- policies used to set permissions boundaries, set the value
    -- to @PermissionsBoundary@.
    --
    -- This parameter is optional. If it is not included, all policies are
    -- returned.
    policyUsageFilter :: Prelude.Maybe PolicyUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefix', 'listPolicies_pathPrefix' - The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'onlyAttached', 'listPolicies_onlyAttached' - A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@, the returned list contains only the
-- policies that are attached to an IAM user, group, or role. When
-- @OnlyAttached@ is @false@, or when the parameter is not included, all
-- policies are returned.
--
-- 'marker', 'listPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'scope', 'listPolicies_scope' - The scope to use for filtering the results.
--
-- To list only Amazon Web Services managed policies, set @Scope@ to @AWS@.
-- To list only the customer managed policies in your Amazon Web Services
-- account, set @Scope@ to @Local@.
--
-- This parameter is optional. If it is not included, or if it is set to
-- @All@, all policies are returned.
--
-- 'maxItems', 'listPolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'policyUsageFilter', 'listPolicies_policyUsageFilter' - The policy usage method to use for filtering the results.
--
-- To list only permissions policies,
-- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
-- policies used to set permissions boundaries, set the value
-- to @PermissionsBoundary@.
--
-- This parameter is optional. If it is not included, all policies are
-- returned.
newListPolicies ::
  ListPolicies
newListPolicies =
  ListPolicies'
    { pathPrefix = Prelude.Nothing,
      onlyAttached = Prelude.Nothing,
      marker = Prelude.Nothing,
      scope = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      policyUsageFilter = Prelude.Nothing
    }

-- | The path prefix for filtering the results. This parameter is optional.
-- If it is not included, it defaults to a slash (\/), listing all
-- policies. This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
listPolicies_pathPrefix :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Text)
listPolicies_pathPrefix = Lens.lens (\ListPolicies' {pathPrefix} -> pathPrefix) (\s@ListPolicies' {} a -> s {pathPrefix = a} :: ListPolicies)

-- | A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@, the returned list contains only the
-- policies that are attached to an IAM user, group, or role. When
-- @OnlyAttached@ is @false@, or when the parameter is not included, all
-- policies are returned.
listPolicies_onlyAttached :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Bool)
listPolicies_onlyAttached = Lens.lens (\ListPolicies' {onlyAttached} -> onlyAttached) (\s@ListPolicies' {} a -> s {onlyAttached = a} :: ListPolicies)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPolicies_marker :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Text)
listPolicies_marker = Lens.lens (\ListPolicies' {marker} -> marker) (\s@ListPolicies' {} a -> s {marker = a} :: ListPolicies)

-- | The scope to use for filtering the results.
--
-- To list only Amazon Web Services managed policies, set @Scope@ to @AWS@.
-- To list only the customer managed policies in your Amazon Web Services
-- account, set @Scope@ to @Local@.
--
-- This parameter is optional. If it is not included, or if it is set to
-- @All@, all policies are returned.
listPolicies_scope :: Lens.Lens' ListPolicies (Prelude.Maybe PolicyScopeType)
listPolicies_scope = Lens.lens (\ListPolicies' {scope} -> scope) (\s@ListPolicies' {} a -> s {scope = a} :: ListPolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listPolicies_maxItems :: Lens.Lens' ListPolicies (Prelude.Maybe Prelude.Natural)
listPolicies_maxItems = Lens.lens (\ListPolicies' {maxItems} -> maxItems) (\s@ListPolicies' {} a -> s {maxItems = a} :: ListPolicies)

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies,
-- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
-- policies used to set permissions boundaries, set the value
-- to @PermissionsBoundary@.
--
-- This parameter is optional. If it is not included, all policies are
-- returned.
listPolicies_policyUsageFilter :: Lens.Lens' ListPolicies (Prelude.Maybe PolicyUsageType)
listPolicies_policyUsageFilter = Lens.lens (\ListPolicies' {policyUsageFilter} -> policyUsageFilter) (\s@ListPolicies' {} a -> s {policyUsageFilter = a} :: ListPolicies)

instance Core.AWSPager ListPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPoliciesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listPoliciesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicies_marker
          Lens..~ rs
          Lens.^? listPoliciesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListPolicies where
  type AWSResponse ListPolicies = ListPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPoliciesResult"
      ( \s h x ->
          ListPoliciesResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> ( x Core..@? "Policies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicies

instance Prelude.NFData ListPolicies

instance Core.ToHeaders ListPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListPolicies" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "PathPrefix" Core.=: pathPrefix,
        "OnlyAttached" Core.=: onlyAttached,
        "Marker" Core.=: marker,
        "Scope" Core.=: scope,
        "MaxItems" Core.=: maxItems,
        "PolicyUsageFilter" Core.=: policyUsageFilter
      ]

-- | Contains the response to a successful ListPolicies request.
--
-- /See:/ 'newListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | A list of policies.
    policies :: Prelude.Maybe [Policy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'listPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'policies', 'listPoliciesResponse_policies' - A list of policies.
--
-- 'httpStatus', 'listPoliciesResponse_httpStatus' - The response's http status code.
newListPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPoliciesResponse
newListPoliciesResponse pHttpStatus_ =
  ListPoliciesResponse'
    { marker = Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      policies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listPoliciesResponse_marker :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe Prelude.Text)
listPoliciesResponse_marker = Lens.lens (\ListPoliciesResponse' {marker} -> marker) (\s@ListPoliciesResponse' {} a -> s {marker = a} :: ListPoliciesResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listPoliciesResponse_isTruncated :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe Prelude.Bool)
listPoliciesResponse_isTruncated = Lens.lens (\ListPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListPoliciesResponse' {} a -> s {isTruncated = a} :: ListPoliciesResponse)

-- | A list of policies.
listPoliciesResponse_policies :: Lens.Lens' ListPoliciesResponse (Prelude.Maybe [Policy])
listPoliciesResponse_policies = Lens.lens (\ListPoliciesResponse' {policies} -> policies) (\s@ListPoliciesResponse' {} a -> s {policies = a} :: ListPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPoliciesResponse_httpStatus :: Lens.Lens' ListPoliciesResponse Prelude.Int
listPoliciesResponse_httpStatus = Lens.lens (\ListPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListPoliciesResponse' {} a -> s {httpStatus = a} :: ListPoliciesResponse)

instance Prelude.NFData ListPoliciesResponse
