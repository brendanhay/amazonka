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
-- Module      : Amazonka.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.IAM.ListAttachedUserPolicies
  ( -- * Creating a Request
    ListAttachedUserPolicies (..),
    newListAttachedUserPolicies,

    -- * Request Lenses
    listAttachedUserPolicies_marker,
    listAttachedUserPolicies_maxItems,
    listAttachedUserPolicies_pathPrefix,
    listAttachedUserPolicies_userName,

    -- * Destructuring the Response
    ListAttachedUserPoliciesResponse (..),
    newListAttachedUserPoliciesResponse,

    -- * Response Lenses
    listAttachedUserPoliciesResponse_attachedPolicies,
    listAttachedUserPoliciesResponse_isTruncated,
    listAttachedUserPoliciesResponse_marker,
    listAttachedUserPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachedUserPolicies' smart constructor.
data ListAttachedUserPolicies = ListAttachedUserPolicies'
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
    -- | The name (friendly name, not ARN) of the user to list attached policies
    -- for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedUserPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listAttachedUserPolicies_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
-- 'userName', 'listAttachedUserPolicies_userName' - The name (friendly name, not ARN) of the user to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListAttachedUserPolicies ::
  -- | 'userName'
  Prelude.Text ->
  ListAttachedUserPolicies
newListAttachedUserPolicies pUserName_ =
  ListAttachedUserPolicies'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      pathPrefix = Prelude.Nothing,
      userName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listAttachedUserPolicies_marker :: Lens.Lens' ListAttachedUserPolicies (Prelude.Maybe Prelude.Text)
listAttachedUserPolicies_marker = Lens.lens (\ListAttachedUserPolicies' {marker} -> marker) (\s@ListAttachedUserPolicies' {} a -> s {marker = a} :: ListAttachedUserPolicies)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listAttachedUserPolicies_maxItems :: Lens.Lens' ListAttachedUserPolicies (Prelude.Maybe Prelude.Natural)
listAttachedUserPolicies_maxItems = Lens.lens (\ListAttachedUserPolicies' {maxItems} -> maxItems) (\s@ListAttachedUserPolicies' {} a -> s {maxItems = a} :: ListAttachedUserPolicies)

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
listAttachedUserPolicies_pathPrefix :: Lens.Lens' ListAttachedUserPolicies (Prelude.Maybe Prelude.Text)
listAttachedUserPolicies_pathPrefix = Lens.lens (\ListAttachedUserPolicies' {pathPrefix} -> pathPrefix) (\s@ListAttachedUserPolicies' {} a -> s {pathPrefix = a} :: ListAttachedUserPolicies)

-- | The name (friendly name, not ARN) of the user to list attached policies
-- for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listAttachedUserPolicies_userName :: Lens.Lens' ListAttachedUserPolicies Prelude.Text
listAttachedUserPolicies_userName = Lens.lens (\ListAttachedUserPolicies' {userName} -> userName) (\s@ListAttachedUserPolicies' {} a -> s {userName = a} :: ListAttachedUserPolicies)

instance Core.AWSPager ListAttachedUserPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedUserPoliciesResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listAttachedUserPoliciesResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAttachedUserPolicies_marker
              Lens..~ rs
              Lens.^? listAttachedUserPoliciesResponse_marker
              Prelude.. Lens._Just

instance Core.AWSRequest ListAttachedUserPolicies where
  type
    AWSResponse ListAttachedUserPolicies =
      ListAttachedUserPoliciesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListAttachedUserPoliciesResult"
      ( \s h x ->
          ListAttachedUserPoliciesResponse'
            Prelude.<$> ( x
                            Data..@? "AttachedPolicies"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachedUserPolicies where
  hashWithSalt _salt ListAttachedUserPolicies' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` pathPrefix
      `Prelude.hashWithSalt` userName

instance Prelude.NFData ListAttachedUserPolicies where
  rnf ListAttachedUserPolicies' {..} =
    Prelude.rnf marker `Prelude.seq`
      Prelude.rnf maxItems `Prelude.seq`
        Prelude.rnf pathPrefix `Prelude.seq`
          Prelude.rnf userName

instance Data.ToHeaders ListAttachedUserPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAttachedUserPolicies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAttachedUserPolicies where
  toQuery ListAttachedUserPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListAttachedUserPolicies" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PathPrefix" Data.=: pathPrefix,
        "UserName" Data.=: userName
      ]

-- | Contains the response to a successful ListAttachedUserPolicies request.
--
-- /See:/ 'newListAttachedUserPoliciesResponse' smart constructor.
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
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
-- Create a value of 'ListAttachedUserPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedPolicies', 'listAttachedUserPoliciesResponse_attachedPolicies' - A list of the attached policies.
--
-- 'isTruncated', 'listAttachedUserPoliciesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listAttachedUserPoliciesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listAttachedUserPoliciesResponse_httpStatus' - The response's http status code.
newListAttachedUserPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachedUserPoliciesResponse
newListAttachedUserPoliciesResponse pHttpStatus_ =
  ListAttachedUserPoliciesResponse'
    { attachedPolicies =
        Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the attached policies.
listAttachedUserPoliciesResponse_attachedPolicies :: Lens.Lens' ListAttachedUserPoliciesResponse (Prelude.Maybe [AttachedPolicy])
listAttachedUserPoliciesResponse_attachedPolicies = Lens.lens (\ListAttachedUserPoliciesResponse' {attachedPolicies} -> attachedPolicies) (\s@ListAttachedUserPoliciesResponse' {} a -> s {attachedPolicies = a} :: ListAttachedUserPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listAttachedUserPoliciesResponse_isTruncated :: Lens.Lens' ListAttachedUserPoliciesResponse (Prelude.Maybe Prelude.Bool)
listAttachedUserPoliciesResponse_isTruncated = Lens.lens (\ListAttachedUserPoliciesResponse' {isTruncated} -> isTruncated) (\s@ListAttachedUserPoliciesResponse' {} a -> s {isTruncated = a} :: ListAttachedUserPoliciesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listAttachedUserPoliciesResponse_marker :: Lens.Lens' ListAttachedUserPoliciesResponse (Prelude.Maybe Prelude.Text)
listAttachedUserPoliciesResponse_marker = Lens.lens (\ListAttachedUserPoliciesResponse' {marker} -> marker) (\s@ListAttachedUserPoliciesResponse' {} a -> s {marker = a} :: ListAttachedUserPoliciesResponse)

-- | The response's http status code.
listAttachedUserPoliciesResponse_httpStatus :: Lens.Lens' ListAttachedUserPoliciesResponse Prelude.Int
listAttachedUserPoliciesResponse_httpStatus = Lens.lens (\ListAttachedUserPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedUserPoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedUserPoliciesResponse)

instance
  Prelude.NFData
    ListAttachedUserPoliciesResponse
  where
  rnf ListAttachedUserPoliciesResponse' {..} =
    Prelude.rnf attachedPolicies `Prelude.seq`
      Prelude.rnf isTruncated `Prelude.seq`
        Prelude.rnf marker `Prelude.seq`
          Prelude.rnf httpStatus
