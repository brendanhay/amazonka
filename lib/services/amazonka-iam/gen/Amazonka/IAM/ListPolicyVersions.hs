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
-- Module      : Amazonka.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the versions of the specified managed policy,
-- including the version that is currently set as the policy\'s default
-- version.
--
-- For more information about managed policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed policies and inline policies>
-- in the /IAM User Guide/.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListPolicyVersions
  ( -- * Creating a Request
    ListPolicyVersions (..),
    newListPolicyVersions,

    -- * Request Lenses
    listPolicyVersions_marker,
    listPolicyVersions_maxItems,
    listPolicyVersions_policyArn,

    -- * Destructuring the Response
    ListPolicyVersionsResponse (..),
    newListPolicyVersionsResponse,

    -- * Response Lenses
    listPolicyVersionsResponse_isTruncated,
    listPolicyVersionsResponse_marker,
    listPolicyVersionsResponse_versions,
    listPolicyVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
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
-- Create a value of 'ListPolicyVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listPolicyVersions_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listPolicyVersions_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'policyArn', 'listPolicyVersions_policyArn' - The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newListPolicyVersions ::
  -- | 'policyArn'
  Prelude.Text ->
  ListPolicyVersions
newListPolicyVersions pPolicyArn_ =
  ListPolicyVersions'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPolicyVersions_marker :: Lens.Lens' ListPolicyVersions (Prelude.Maybe Prelude.Text)
listPolicyVersions_marker = Lens.lens (\ListPolicyVersions' {marker} -> marker) (\s@ListPolicyVersions' {} a -> s {marker = a} :: ListPolicyVersions)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listPolicyVersions_maxItems :: Lens.Lens' ListPolicyVersions (Prelude.Maybe Prelude.Natural)
listPolicyVersions_maxItems = Lens.lens (\ListPolicyVersions' {maxItems} -> maxItems) (\s@ListPolicyVersions' {} a -> s {maxItems = a} :: ListPolicyVersions)

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
listPolicyVersions_policyArn :: Lens.Lens' ListPolicyVersions Prelude.Text
listPolicyVersions_policyArn = Lens.lens (\ListPolicyVersions' {policyArn} -> policyArn) (\s@ListPolicyVersions' {} a -> s {policyArn = a} :: ListPolicyVersions)

instance Core.AWSPager ListPolicyVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPolicyVersionsResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listPolicyVersionsResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPolicyVersions_marker
          Lens..~ rs
          Lens.^? listPolicyVersionsResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyVersions where
  type
    AWSResponse ListPolicyVersions =
      ListPolicyVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListPolicyVersionsResult"
      ( \s h x ->
          ListPolicyVersionsResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> ( x
                            Data..@? "Versions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicyVersions where
  hashWithSalt _salt ListPolicyVersions' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` policyArn

instance Prelude.NFData ListPolicyVersions where
  rnf ListPolicyVersions' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf policyArn

instance Data.ToHeaders ListPolicyVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPolicyVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPolicyVersions where
  toQuery ListPolicyVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListPolicyVersions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "PolicyArn" Data.=: policyArn
      ]

-- | Contains the response to a successful ListPolicyVersions request.
--
-- /See:/ 'newListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
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
    -- | A list of policy versions.
    --
    -- For more information about managed policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    versions :: Prelude.Maybe [PolicyVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listPolicyVersionsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listPolicyVersionsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'versions', 'listPolicyVersionsResponse_versions' - A list of policy versions.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
--
-- 'httpStatus', 'listPolicyVersionsResponse_httpStatus' - The response's http status code.
newListPolicyVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyVersionsResponse
newListPolicyVersionsResponse pHttpStatus_ =
  ListPolicyVersionsResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      versions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listPolicyVersionsResponse_isTruncated :: Lens.Lens' ListPolicyVersionsResponse (Prelude.Maybe Prelude.Bool)
listPolicyVersionsResponse_isTruncated = Lens.lens (\ListPolicyVersionsResponse' {isTruncated} -> isTruncated) (\s@ListPolicyVersionsResponse' {} a -> s {isTruncated = a} :: ListPolicyVersionsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listPolicyVersionsResponse_marker :: Lens.Lens' ListPolicyVersionsResponse (Prelude.Maybe Prelude.Text)
listPolicyVersionsResponse_marker = Lens.lens (\ListPolicyVersionsResponse' {marker} -> marker) (\s@ListPolicyVersionsResponse' {} a -> s {marker = a} :: ListPolicyVersionsResponse)

-- | A list of policy versions.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
listPolicyVersionsResponse_versions :: Lens.Lens' ListPolicyVersionsResponse (Prelude.Maybe [PolicyVersion])
listPolicyVersionsResponse_versions = Lens.lens (\ListPolicyVersionsResponse' {versions} -> versions) (\s@ListPolicyVersionsResponse' {} a -> s {versions = a} :: ListPolicyVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPolicyVersionsResponse_httpStatus :: Lens.Lens' ListPolicyVersionsResponse Prelude.Int
listPolicyVersionsResponse_httpStatus = Lens.lens (\ListPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListPolicyVersionsResponse)

instance Prelude.NFData ListPolicyVersionsResponse where
  rnf ListPolicyVersionsResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
