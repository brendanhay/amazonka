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
-- Module      : Network.AWS.IAM.ListPolicyVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.IAM.ListPolicyVersions
  ( -- * Creating a Request
    ListPolicyVersions (..),
    newListPolicyVersions,

    -- * Request Lenses
    listPolicyVersions_maxItems,
    listPolicyVersions_marker,
    listPolicyVersions_policyArn,

    -- * Destructuring the Response
    ListPolicyVersionsResponse (..),
    newListPolicyVersionsResponse,

    -- * Response Lenses
    listPolicyVersionsResponse_versions,
    listPolicyVersionsResponse_isTruncated,
    listPolicyVersionsResponse_marker,
    listPolicyVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { -- | Use this only when paginating results to indicate the maximum number of
    -- items you want in the response. If additional items exist beyond the
    -- maximum you specify, the @IsTruncated@ response element is @true@.
    --
    -- If you do not include this parameter, the number of items defaults to
    -- 100. Note that IAM might return fewer results, even when there are more
    -- results available. In that case, the @IsTruncated@ response element
    -- returns @true@, and @Marker@ contains a value to include in the
    -- subsequent call that tells the service where to continue from.
    maxItems :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
    -- versions.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
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
-- 'marker', 'listPolicyVersions_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'policyArn', 'listPolicyVersions_policyArn' - The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newListPolicyVersions ::
  -- | 'policyArn'
  Prelude.Text ->
  ListPolicyVersions
newListPolicyVersions pPolicyArn_ =
  ListPolicyVersions'
    { maxItems = Prelude.Nothing,
      marker = Prelude.Nothing,
      policyArn = pPolicyArn_
    }

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

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listPolicyVersions_marker :: Lens.Lens' ListPolicyVersions (Prelude.Maybe Prelude.Text)
listPolicyVersions_marker = Lens.lens (\ListPolicyVersions' {marker} -> marker) (\s@ListPolicyVersions' {} a -> s {marker = a} :: ListPolicyVersions)

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the
-- versions.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
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
      Prelude.Just Prelude.$
        rq
          Prelude.& listPolicyVersions_marker
          Lens..~ rs
          Lens.^? listPolicyVersionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListPolicyVersions where
  type
    AWSResponse ListPolicyVersions =
      ListPolicyVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListPolicyVersionsResult"
      ( \s h x ->
          ListPolicyVersionsResponse'
            Prelude.<$> ( x Core..@? "Versions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicyVersions

instance Prelude.NFData ListPolicyVersions

instance Core.ToHeaders ListPolicyVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListPolicyVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListPolicyVersions where
  toQuery ListPolicyVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListPolicyVersions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker,
        "PolicyArn" Core.=: policyArn
      ]

-- | Contains the response to a successful ListPolicyVersions request.
--
-- /See:/ 'newListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { -- | A list of policy versions.
    --
    -- For more information about managed policy versions, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
    -- in the /IAM User Guide/.
    versions :: Prelude.Maybe [PolicyVersion],
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
-- Create a value of 'ListPolicyVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'listPolicyVersionsResponse_versions' - A list of policy versions.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
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
-- 'httpStatus', 'listPolicyVersionsResponse_httpStatus' - The response's http status code.
newListPolicyVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyVersionsResponse
newListPolicyVersionsResponse pHttpStatus_ =
  ListPolicyVersionsResponse'
    { versions =
        Prelude.Nothing,
      isTruncated = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of policy versions.
--
-- For more information about managed policy versions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for managed policies>
-- in the /IAM User Guide/.
listPolicyVersionsResponse_versions :: Lens.Lens' ListPolicyVersionsResponse (Prelude.Maybe [PolicyVersion])
listPolicyVersionsResponse_versions = Lens.lens (\ListPolicyVersionsResponse' {versions} -> versions) (\s@ListPolicyVersionsResponse' {} a -> s {versions = a} :: ListPolicyVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

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

-- | The response's http status code.
listPolicyVersionsResponse_httpStatus :: Lens.Lens' ListPolicyVersionsResponse Prelude.Int
listPolicyVersionsResponse_httpStatus = Lens.lens (\ListPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListPolicyVersionsResponse)

instance Prelude.NFData ListPolicyVersionsResponse
