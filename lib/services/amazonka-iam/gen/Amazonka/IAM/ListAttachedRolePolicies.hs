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
-- Module      : Amazonka.IAM.ListAttachedRolePolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM role.
--
-- An IAM role can also have inline policies embedded with it. To list the
-- inline policies for a role, use ListRolePolicies. For information about
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters. You can use the @PathPrefix@ parameter to limit the list of
-- policies to only those matching the specified path prefix. If there are
-- no policies attached to the specified role (or none that match the
-- specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListAttachedRolePolicies
  ( -- * Creating a Request
    ListAttachedRolePolicies (..),
    newListAttachedRolePolicies,

    -- * Request Lenses
    listAttachedRolePolicies_marker,
    listAttachedRolePolicies_maxItems,
    listAttachedRolePolicies_pathPrefix,
    listAttachedRolePolicies_roleName,

    -- * Destructuring the Response
    ListAttachedRolePoliciesResponse (..),
    newListAttachedRolePoliciesResponse,

    -- * Response Lenses
    listAttachedRolePoliciesResponse_attachedPolicies,
    listAttachedRolePoliciesResponse_isTruncated,
    listAttachedRolePoliciesResponse_marker,
    listAttachedRolePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachedRolePolicies' smart constructor.
data ListAttachedRolePolicies = ListAttachedRolePolicies'
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
    pathPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name (friendly name, not ARN) of the role to list attached policies
    -- for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedRolePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listAttachedRolePolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listAttachedRolePolicies_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'pathPrefix', 'listAttachedRolePolicies_pathPrefix' - The path prefix for filtering the results. This parameter is optional.
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
-- 'roleName', 'listAttachedRolePolicies_roleName' - The name (friendly name, not ARN) of the role to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListAttachedRolePolicies ::
  -- | 'roleName'
  Prelude.Text ->
  ListAttachedRolePolicies
newListAttachedRolePolicies pRoleName_ =
  ListAttachedRolePolicies'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      pathPrefix = Prelude.Nothing,
      roleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAttachedRolePolicies_marker :: Lens.Lens' ListAttachedRolePolicies (Prelude.Maybe Prelude.Text)
listAttachedRolePolicies_marker = Lens.lens (\ListAttachedRolePolicies' {marker} -> marker) (\s@ListAttachedRolePolicies' {} a -> s {marker = a} :: ListAttachedRolePolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAttachedRolePolicies_maxItems :: Lens.Lens' ListAttachedRolePolicies (Prelude.Maybe Prelude.Natural)
listAttachedRolePolicies_maxItems = Lens.lens (\ListAttachedRolePolicies' {maxItems} -> maxItems) (\s@ListAttachedRolePolicies' {} a -> s {maxItems = a} :: ListAttachedRolePolicies)

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
listAttachedRolePolicies_pathPrefix :: Lens.Lens' ListAttachedRolePolicies (Prelude.Maybe Prelude.Text)
listAttachedRolePolicies_pathPrefix = Lens.lens (\ListAttachedRolePolicies' {pathPrefix} -> pathPrefix) (\s@ListAttachedRolePolicies' {} a -> s {pathPrefix = a} :: ListAttachedRolePolicies)

-- | The name (friendly name, not ARN) of the role to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAttachedRolePolicies_roleName :: Lens.Lens' ListAttachedRolePolicies Prelude.Text
listAttachedRolePolicies_roleName = Lens.lens (\ListAttachedRolePolicies' {roleName} -> roleName) (\s@ListAttachedRolePolicies' {} a -> s {roleName = a} :: ListAttachedRolePolicies)

instance Core.AWSPager ListAttachedRolePolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedRolePoliciesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listAttachedRolePoliciesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttachedRolePolicies_marker
          Lens..~ rs
          Lens.^? listAttachedRolePoliciesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListAttachedRolePolicies where
  type
    AWSResponse ListAttachedRolePolicies =
      ListAttachedRolePoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListAttachedRolePoliciesResult"
      ( \s h x ->
          ListAttachedRolePoliciesResponse'
            Prelude.<$> ( x Data..@? "AttachedPolicies"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachedRolePolicies where
  hashWithSalt _salt ListAttachedRolePolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` pathPrefix
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData ListAttachedRolePolicies where
  rnf ListAttachedRolePolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf pathPrefix
      `Prelude.seq` Prelude.rnf roleName

instance Data.ToHeaders ListAttachedRolePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAttachedRolePolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAttachedRolePolicies where
  toQuery ListAttachedRolePolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListAttachedRolePolicies" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PathPrefix" Data.=: pathPrefix,
        "RoleName" Data.=: roleName
      ]

-- | Contains the response to a successful ListAttachedRolePolicies request.
--
-- /See:/ 'newListAttachedRolePoliciesResponse' smart constructor.
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'
  { -- | A list of the attached policies.
    attachedPolicies :: Prelude.Maybe [AttachedPolicy],
    -- | A flag that indicates whether there are more items to return. If your
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
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedRolePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedPolicies', 'listAttachedRolePoliciesResponse_attachedPolicies' - A list of the attached policies.
--
-- 'isTruncated', 'listAttachedRolePoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listAttachedRolePoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAttachedRolePoliciesResponse_httpStatus' - The response's http status code.
newListAttachedRolePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachedRolePoliciesResponse
newListAttachedRolePoliciesResponse pHttpStatus_ =
  ListAttachedRolePoliciesResponse'
    { attachedPolicies =
        Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the attached policies.
listAttachedRolePoliciesResponse_attachedPolicies :: Lens.Lens' ListAttachedRolePoliciesResponse (Prelude.Maybe [AttachedPolicy])
listAttachedRolePoliciesResponse_attachedPolicies = Lens.lens (\ListAttachedRolePoliciesResponse' {attachedPolicies} -> attachedPolicies) (\s@ListAttachedRolePoliciesResponse' {} a -> s {attachedPolicies = a} :: ListAttachedRolePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAttachedRolePoliciesResponse_isTruncated :: Lens.Lens' ListAttachedRolePoliciesResponse (Prelude.Maybe Prelude.Bool)
listAttachedRolePoliciesResponse_isTruncated = Lens.lens (\ListAttachedRolePoliciesResponse' {isTruncated} -> isTruncated) (\s@ListAttachedRolePoliciesResponse' {} a -> s {isTruncated = a} :: ListAttachedRolePoliciesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAttachedRolePoliciesResponse_marker :: Lens.Lens' ListAttachedRolePoliciesResponse (Prelude.Maybe Prelude.Text)
listAttachedRolePoliciesResponse_marker = Lens.lens (\ListAttachedRolePoliciesResponse' {marker} -> marker) (\s@ListAttachedRolePoliciesResponse' {} a -> s {marker = a} :: ListAttachedRolePoliciesResponse)

-- | The response's http status code.
listAttachedRolePoliciesResponse_httpStatus :: Lens.Lens' ListAttachedRolePoliciesResponse Prelude.Int
listAttachedRolePoliciesResponse_httpStatus = Lens.lens (\ListAttachedRolePoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedRolePoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedRolePoliciesResponse)

instance
  Prelude.NFData
    ListAttachedRolePoliciesResponse
  where
  rnf ListAttachedRolePoliciesResponse' {..} =
    Prelude.rnf attachedPolicies
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
