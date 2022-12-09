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
-- Module      : Amazonka.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IAM.ListEntitiesForPolicy
  ( -- * Creating a Request
    ListEntitiesForPolicy (..),
    newListEntitiesForPolicy,

    -- * Request Lenses
    listEntitiesForPolicy_entityFilter,
    listEntitiesForPolicy_marker,
    listEntitiesForPolicy_maxItems,
    listEntitiesForPolicy_pathPrefix,
    listEntitiesForPolicy_policyUsageFilter,
    listEntitiesForPolicy_policyArn,

    -- * Destructuring the Response
    ListEntitiesForPolicyResponse (..),
    newListEntitiesForPolicyResponse,

    -- * Response Lenses
    listEntitiesForPolicyResponse_isTruncated,
    listEntitiesForPolicyResponse_marker,
    listEntitiesForPolicyResponse_policyGroups,
    listEntitiesForPolicyResponse_policyRoles,
    listEntitiesForPolicyResponse_policyUsers,
    listEntitiesForPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { -- | The entity type to use for filtering the results.
    --
    -- For example, when @EntityFilter@ is @Role@, only the roles that are
    -- attached to the specified policy are returned. This parameter is
    -- optional. If it is not included, all attached entities (users, groups,
    -- and roles) are returned. The argument for this parameter must be one of
    -- the valid values listed below.
    entityFilter :: Prelude.Maybe EntityType,
    -- | Use this parameter only when paginating results and only after you
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
    pathPrefix :: Prelude.Maybe Prelude.Text,
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies,
    -- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
    -- policies used to set permissions boundaries, set the value
    -- to @PermissionsBoundary@.
    --
    -- This parameter is optional. If it is not included, all policies are
    -- returned.
    policyUsageFilter :: Prelude.Maybe PolicyUsageType,
    -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
    -- versions.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'marker', 'listEntitiesForPolicy_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
-- 'policyArn', 'listEntitiesForPolicy_policyArn' - The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newListEntitiesForPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  ListEntitiesForPolicy
newListEntitiesForPolicy pPolicyArn_ =
  ListEntitiesForPolicy'
    { entityFilter =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      pathPrefix = Prelude.Nothing,
      policyUsageFilter = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@, only the roles that are
-- attached to the specified policy are returned. This parameter is
-- optional. If it is not included, all attached entities (users, groups,
-- and roles) are returned. The argument for this parameter must be one of
-- the valid values listed below.
listEntitiesForPolicy_entityFilter :: Lens.Lens' ListEntitiesForPolicy (Prelude.Maybe EntityType)
listEntitiesForPolicy_entityFilter = Lens.lens (\ListEntitiesForPolicy' {entityFilter} -> entityFilter) (\s@ListEntitiesForPolicy' {} a -> s {entityFilter = a} :: ListEntitiesForPolicy)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listEntitiesForPolicy_marker :: Lens.Lens' ListEntitiesForPolicy (Prelude.Maybe Prelude.Text)
listEntitiesForPolicy_marker = Lens.lens (\ListEntitiesForPolicy' {marker} -> marker) (\s@ListEntitiesForPolicy' {} a -> s {marker = a} :: ListEntitiesForPolicy)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listEntitiesForPolicy_maxItems :: Lens.Lens' ListEntitiesForPolicy (Prelude.Maybe Prelude.Natural)
listEntitiesForPolicy_maxItems = Lens.lens (\ListEntitiesForPolicy' {maxItems} -> maxItems) (\s@ListEntitiesForPolicy' {} a -> s {maxItems = a} :: ListEntitiesForPolicy)

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
listEntitiesForPolicy_pathPrefix :: Lens.Lens' ListEntitiesForPolicy (Prelude.Maybe Prelude.Text)
listEntitiesForPolicy_pathPrefix = Lens.lens (\ListEntitiesForPolicy' {pathPrefix} -> pathPrefix) (\s@ListEntitiesForPolicy' {} a -> s {pathPrefix = a} :: ListEntitiesForPolicy)

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies,
-- set @PolicyUsageFilter@ to @PermissionsPolicy@. To list only the
-- policies used to set permissions boundaries, set the value
-- to @PermissionsBoundary@.
--
-- This parameter is optional. If it is not included, all policies are
-- returned.
listEntitiesForPolicy_policyUsageFilter :: Lens.Lens' ListEntitiesForPolicy (Prelude.Maybe PolicyUsageType)
listEntitiesForPolicy_policyUsageFilter = Lens.lens (\ListEntitiesForPolicy' {policyUsageFilter} -> policyUsageFilter) (\s@ListEntitiesForPolicy' {} a -> s {policyUsageFilter = a} :: ListEntitiesForPolicy)

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
listEntitiesForPolicy_policyArn :: Lens.Lens' ListEntitiesForPolicy Prelude.Text
listEntitiesForPolicy_policyArn = Lens.lens (\ListEntitiesForPolicy' {policyArn} -> policyArn) (\s@ListEntitiesForPolicy' {} a -> s {policyArn = a} :: ListEntitiesForPolicy)

instance Core.AWSPager ListEntitiesForPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEntitiesForPolicyResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listEntitiesForPolicyResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEntitiesForPolicy_marker
          Lens..~ rs
          Lens.^? listEntitiesForPolicyResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListEntitiesForPolicy where
  type
    AWSResponse ListEntitiesForPolicy =
      ListEntitiesForPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListEntitiesForPolicyResult"
      ( \s h x ->
          ListEntitiesForPolicyResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "PolicyGroups" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "PolicyRoles" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "PolicyUsers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitiesForPolicy where
  hashWithSalt _salt ListEntitiesForPolicy' {..} =
    _salt `Prelude.hashWithSalt` entityFilter
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` pathPrefix
      `Prelude.hashWithSalt` policyUsageFilter
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData ListEntitiesForPolicy where
  rnf ListEntitiesForPolicy' {..} =
    Prelude.rnf entityFilter
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf pathPrefix
      `Prelude.seq` Prelude.rnf policyUsageFilter
      `Prelude.seq` Prelude.rnf policyArn

instance Data.ToHeaders ListEntitiesForPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListEntitiesForPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntitiesForPolicy where
  toQuery ListEntitiesForPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListEntitiesForPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "EntityFilter" Data.=: entityFilter,
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PathPrefix" Data.=: pathPrefix,
        "PolicyUsageFilter" Data.=: policyUsageFilter,
        "PolicyArn" Data.=: policyArn
      ]

-- | Contains the response to a successful ListEntitiesForPolicy request.
--
-- /See:/ 'newListEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
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
    -- | A list of IAM groups that the policy is attached to.
    policyGroups :: Prelude.Maybe [PolicyGroup],
    -- | A list of IAM roles that the policy is attached to.
    policyRoles :: Prelude.Maybe [PolicyRole],
    -- | A list of IAM users that the policy is attached to.
    policyUsers :: Prelude.Maybe [PolicyUser],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitiesForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listEntitiesForPolicyResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listEntitiesForPolicyResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'policyGroups', 'listEntitiesForPolicyResponse_policyGroups' - A list of IAM groups that the policy is attached to.
--
-- 'policyRoles', 'listEntitiesForPolicyResponse_policyRoles' - A list of IAM roles that the policy is attached to.
--
-- 'policyUsers', 'listEntitiesForPolicyResponse_policyUsers' - A list of IAM users that the policy is attached to.
--
-- 'httpStatus', 'listEntitiesForPolicyResponse_httpStatus' - The response's http status code.
newListEntitiesForPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitiesForPolicyResponse
newListEntitiesForPolicyResponse pHttpStatus_ =
  ListEntitiesForPolicyResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      policyGroups = Prelude.Nothing,
      policyRoles = Prelude.Nothing,
      policyUsers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listEntitiesForPolicyResponse_isTruncated :: Lens.Lens' ListEntitiesForPolicyResponse (Prelude.Maybe Prelude.Bool)
listEntitiesForPolicyResponse_isTruncated = Lens.lens (\ListEntitiesForPolicyResponse' {isTruncated} -> isTruncated) (\s@ListEntitiesForPolicyResponse' {} a -> s {isTruncated = a} :: ListEntitiesForPolicyResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listEntitiesForPolicyResponse_marker :: Lens.Lens' ListEntitiesForPolicyResponse (Prelude.Maybe Prelude.Text)
listEntitiesForPolicyResponse_marker = Lens.lens (\ListEntitiesForPolicyResponse' {marker} -> marker) (\s@ListEntitiesForPolicyResponse' {} a -> s {marker = a} :: ListEntitiesForPolicyResponse)

-- | A list of IAM groups that the policy is attached to.
listEntitiesForPolicyResponse_policyGroups :: Lens.Lens' ListEntitiesForPolicyResponse (Prelude.Maybe [PolicyGroup])
listEntitiesForPolicyResponse_policyGroups = Lens.lens (\ListEntitiesForPolicyResponse' {policyGroups} -> policyGroups) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyGroups = a} :: ListEntitiesForPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of IAM roles that the policy is attached to.
listEntitiesForPolicyResponse_policyRoles :: Lens.Lens' ListEntitiesForPolicyResponse (Prelude.Maybe [PolicyRole])
listEntitiesForPolicyResponse_policyRoles = Lens.lens (\ListEntitiesForPolicyResponse' {policyRoles} -> policyRoles) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyRoles = a} :: ListEntitiesForPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of IAM users that the policy is attached to.
listEntitiesForPolicyResponse_policyUsers :: Lens.Lens' ListEntitiesForPolicyResponse (Prelude.Maybe [PolicyUser])
listEntitiesForPolicyResponse_policyUsers = Lens.lens (\ListEntitiesForPolicyResponse' {policyUsers} -> policyUsers) (\s@ListEntitiesForPolicyResponse' {} a -> s {policyUsers = a} :: ListEntitiesForPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEntitiesForPolicyResponse_httpStatus :: Lens.Lens' ListEntitiesForPolicyResponse Prelude.Int
listEntitiesForPolicyResponse_httpStatus = Lens.lens (\ListEntitiesForPolicyResponse' {httpStatus} -> httpStatus) (\s@ListEntitiesForPolicyResponse' {} a -> s {httpStatus = a} :: ListEntitiesForPolicyResponse)

instance Prelude.NFData ListEntitiesForPolicyResponse where
  rnf ListEntitiesForPolicyResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf policyGroups
      `Prelude.seq` Prelude.rnf policyRoles
      `Prelude.seq` Prelude.rnf policyUsers
      `Prelude.seq` Prelude.rnf httpStatus
