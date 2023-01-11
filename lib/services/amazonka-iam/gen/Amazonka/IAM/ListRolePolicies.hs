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
-- Module      : Amazonka.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the
-- specified IAM role.
--
-- An IAM role can also have managed policies attached to it. To list the
-- managed policies that are attached to a role, use
-- ListAttachedRolePolicies. For more information about policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. If there are no inline policies embedded with the specified
-- role, the operation returns an empty list.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListRolePolicies
  ( -- * Creating a Request
    ListRolePolicies (..),
    newListRolePolicies,

    -- * Request Lenses
    listRolePolicies_marker,
    listRolePolicies_maxItems,
    listRolePolicies_roleName,

    -- * Destructuring the Response
    ListRolePoliciesResponse (..),
    newListRolePoliciesResponse,

    -- * Response Lenses
    listRolePoliciesResponse_isTruncated,
    listRolePoliciesResponse_marker,
    listRolePoliciesResponse_httpStatus,
    listRolePoliciesResponse_policyNames,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRolePolicies' smart constructor.
data ListRolePolicies = ListRolePolicies'
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
    -- | The name of the role to list policies for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRolePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listRolePolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listRolePolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'roleName', 'listRolePolicies_roleName' - The name of the role to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListRolePolicies ::
  -- | 'roleName'
  Prelude.Text ->
  ListRolePolicies
newListRolePolicies pRoleName_ =
  ListRolePolicies'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      roleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listRolePolicies_marker :: Lens.Lens' ListRolePolicies (Prelude.Maybe Prelude.Text)
listRolePolicies_marker = Lens.lens (\ListRolePolicies' {marker} -> marker) (\s@ListRolePolicies' {} a -> s {marker = a} :: ListRolePolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listRolePolicies_maxItems :: Lens.Lens' ListRolePolicies (Prelude.Maybe Prelude.Natural)
listRolePolicies_maxItems = Lens.lens (\ListRolePolicies' {maxItems} -> maxItems) (\s@ListRolePolicies' {} a -> s {maxItems = a} :: ListRolePolicies)

-- | The name of the role to list policies for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listRolePolicies_roleName :: Lens.Lens' ListRolePolicies Prelude.Text
listRolePolicies_roleName = Lens.lens (\ListRolePolicies' {roleName} -> roleName) (\s@ListRolePolicies' {} a -> s {roleName = a} :: ListRolePolicies)

instance Core.AWSPager ListRolePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRolePoliciesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listRolePoliciesResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRolePolicies_marker
          Lens..~ rs
          Lens.^? listRolePoliciesResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest ListRolePolicies where
  type
    AWSResponse ListRolePolicies =
      ListRolePoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListRolePoliciesResult"
      ( \s h x ->
          ListRolePoliciesResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "PolicyNames" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListRolePolicies where
  hashWithSalt _salt ListRolePolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData ListRolePolicies where
  rnf ListRolePolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf roleName

instance Data.ToHeaders ListRolePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListRolePolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRolePolicies where
  toQuery ListRolePolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListRolePolicies" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "RoleName" Data.=: roleName
      ]

-- | Contains the response to a successful ListRolePolicies request.
--
-- /See:/ 'newListRolePoliciesResponse' smart constructor.
data ListRolePoliciesResponse = ListRolePoliciesResponse'
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
-- Create a value of 'ListRolePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listRolePoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listRolePoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listRolePoliciesResponse_httpStatus' - The response's http status code.
--
-- 'policyNames', 'listRolePoliciesResponse_policyNames' - A list of policy names.
newListRolePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRolePoliciesResponse
newListRolePoliciesResponse pHttpStatus_ =
  ListRolePoliciesResponse'
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
listRolePoliciesResponse_isTruncated :: Lens.Lens' ListRolePoliciesResponse (Prelude.Maybe Prelude.Bool)
listRolePoliciesResponse_isTruncated = Lens.lens (\ListRolePoliciesResponse' {isTruncated} -> isTruncated) (\s@ListRolePoliciesResponse' {} a -> s {isTruncated = a} :: ListRolePoliciesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listRolePoliciesResponse_marker :: Lens.Lens' ListRolePoliciesResponse (Prelude.Maybe Prelude.Text)
listRolePoliciesResponse_marker = Lens.lens (\ListRolePoliciesResponse' {marker} -> marker) (\s@ListRolePoliciesResponse' {} a -> s {marker = a} :: ListRolePoliciesResponse)

-- | The response's http status code.
listRolePoliciesResponse_httpStatus :: Lens.Lens' ListRolePoliciesResponse Prelude.Int
listRolePoliciesResponse_httpStatus = Lens.lens (\ListRolePoliciesResponse' {httpStatus} -> httpStatus) (\s@ListRolePoliciesResponse' {} a -> s {httpStatus = a} :: ListRolePoliciesResponse)

-- | A list of policy names.
listRolePoliciesResponse_policyNames :: Lens.Lens' ListRolePoliciesResponse [Prelude.Text]
listRolePoliciesResponse_policyNames = Lens.lens (\ListRolePoliciesResponse' {policyNames} -> policyNames) (\s@ListRolePoliciesResponse' {} a -> s {policyNames = a} :: ListRolePoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRolePoliciesResponse where
  rnf ListRolePoliciesResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyNames
