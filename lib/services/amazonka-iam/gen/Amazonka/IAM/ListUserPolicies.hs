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
-- Module      : Amazonka.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.IAM.ListUserPolicies
  ( -- * Creating a Request
    ListUserPolicies (..),
    newListUserPolicies,

    -- * Request Lenses
    listUserPolicies_marker,
    listUserPolicies_maxItems,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUserPolicies' smart constructor.
data ListUserPolicies = ListUserPolicies'
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
-- 'marker', 'listUserPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      userName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listUserPolicies_marker :: Lens.Lens' ListUserPolicies (Prelude.Maybe Prelude.Text)
listUserPolicies_marker = Lens.lens (\ListUserPolicies' {marker} -> marker) (\s@ListUserPolicies' {} a -> s {marker = a} :: ListUserPolicies)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListUserPoliciesResult"
      ( \s h x ->
          ListUserPoliciesResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "PolicyNames" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListUserPolicies where
  hashWithSalt _salt ListUserPolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListUserPolicies where
  rnf ListUserPolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders ListUserPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListUserPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUserPolicies where
  toQuery ListUserPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListUserPolicies" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "UserName" Data.=: userName
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
listUserPoliciesResponse_policyNames = Lens.lens (\ListUserPoliciesResponse' {policyNames} -> policyNames) (\s@ListUserPoliciesResponse' {} a -> s {policyNames = a} :: ListUserPoliciesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListUserPoliciesResponse where
  rnf ListUserPoliciesResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policyNames
