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
-- Module      : Network.AWS.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all IAM users, groups, roles, and policies
-- in your AWS account, including their relationships to one another. Use
-- this operation to obtain a snapshot of the configuration of IAM
-- permissions (users, groups, roles, and policies) in your account.
--
-- Policies returned by this operation are URL-encoded compliant with
-- <https://tools.ietf.org/html/rfc3986 RFC 3986>. You can use a URL
-- decoding method to convert the policy back to plain JSON text. For
-- example, if you use Java, you can use the @decode@ method of the
-- @java.net.URLDecoder@ utility class in the Java SDK. Other languages and
-- SDKs provide similar functionality.
--
-- You can optionally filter the results using the @Filter@ parameter. You
-- can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.GetAccountAuthorizationDetails
  ( -- * Creating a Request
    GetAccountAuthorizationDetails (..),
    newGetAccountAuthorizationDetails,

    -- * Request Lenses
    getAccountAuthorizationDetails_filter,
    getAccountAuthorizationDetails_maxItems,
    getAccountAuthorizationDetails_marker,

    -- * Destructuring the Response
    GetAccountAuthorizationDetailsResponse (..),
    newGetAccountAuthorizationDetailsResponse,

    -- * Response Lenses
    getAccountAuthorizationDetailsResponse_roleDetailList,
    getAccountAuthorizationDetailsResponse_groupDetailList,
    getAccountAuthorizationDetailsResponse_policies,
    getAccountAuthorizationDetailsResponse_isTruncated,
    getAccountAuthorizationDetailsResponse_userDetailList,
    getAccountAuthorizationDetailsResponse_marker,
    getAccountAuthorizationDetailsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountAuthorizationDetails' smart constructor.
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'
  { -- | A list of entity types used to filter the results. Only the entities
    -- that match the types you specify are included in the output. Use the
    -- value @LocalManagedPolicy@ to include customer managed policies.
    --
    -- The format for this parameter is a comma-separated (if more than one)
    -- list of strings. Each string value in the list must be one of the valid
    -- values listed below.
    filter' :: Prelude.Maybe [EntityType],
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
    -- | Use this parameter only when paginating results and only after you
    -- receive a response indicating that the results are truncated. Set it to
    -- the value of the @Marker@ element in the response that you received to
    -- indicate where the next call should start.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountAuthorizationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'getAccountAuthorizationDetails_filter' - A list of entity types used to filter the results. Only the entities
-- that match the types you specify are included in the output. Use the
-- value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one)
-- list of strings. Each string value in the list must be one of the valid
-- values listed below.
--
-- 'maxItems', 'getAccountAuthorizationDetails_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'getAccountAuthorizationDetails_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newGetAccountAuthorizationDetails ::
  GetAccountAuthorizationDetails
newGetAccountAuthorizationDetails =
  GetAccountAuthorizationDetails'
    { filter' =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | A list of entity types used to filter the results. Only the entities
-- that match the types you specify are included in the output. Use the
-- value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one)
-- list of strings. Each string value in the list must be one of the valid
-- values listed below.
getAccountAuthorizationDetails_filter :: Lens.Lens' GetAccountAuthorizationDetails (Prelude.Maybe [EntityType])
getAccountAuthorizationDetails_filter = Lens.lens (\GetAccountAuthorizationDetails' {filter'} -> filter') (\s@GetAccountAuthorizationDetails' {} a -> s {filter' = a} :: GetAccountAuthorizationDetails) Prelude.. Lens.mapping Lens._Coerce

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
getAccountAuthorizationDetails_maxItems :: Lens.Lens' GetAccountAuthorizationDetails (Prelude.Maybe Prelude.Natural)
getAccountAuthorizationDetails_maxItems = Lens.lens (\GetAccountAuthorizationDetails' {maxItems} -> maxItems) (\s@GetAccountAuthorizationDetails' {} a -> s {maxItems = a} :: GetAccountAuthorizationDetails)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
getAccountAuthorizationDetails_marker :: Lens.Lens' GetAccountAuthorizationDetails (Prelude.Maybe Prelude.Text)
getAccountAuthorizationDetails_marker = Lens.lens (\GetAccountAuthorizationDetails' {marker} -> marker) (\s@GetAccountAuthorizationDetails' {} a -> s {marker = a} :: GetAccountAuthorizationDetails)

instance Core.AWSPager GetAccountAuthorizationDetails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getAccountAuthorizationDetailsResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? getAccountAuthorizationDetailsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getAccountAuthorizationDetails_marker
          Lens..~ rs
          Lens.^? getAccountAuthorizationDetailsResponse_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetAccountAuthorizationDetails
  where
  type
    AWSResponse GetAccountAuthorizationDetails =
      GetAccountAuthorizationDetailsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccountAuthorizationDetailsResult"
      ( \s h x ->
          GetAccountAuthorizationDetailsResponse'
            Prelude.<$> ( x Core..@? "RoleDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "GroupDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "Policies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "IsTruncated")
            Prelude.<*> ( x Core..@? "UserDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAccountAuthorizationDetails

instance
  Prelude.NFData
    GetAccountAuthorizationDetails

instance
  Core.ToHeaders
    GetAccountAuthorizationDetails
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetAccountAuthorizationDetails where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAccountAuthorizationDetails where
  toQuery GetAccountAuthorizationDetails' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetAccountAuthorizationDetails" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "Filter"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> filter'),
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful GetAccountAuthorizationDetails
-- request.
--
-- /See:/ 'newGetAccountAuthorizationDetailsResponse' smart constructor.
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'
  { -- | A list containing information about IAM roles.
    roleDetailList :: Prelude.Maybe [RoleDetail],
    -- | A list containing information about IAM groups.
    groupDetailList :: Prelude.Maybe [GroupDetail],
    -- | A list containing information about managed policies.
    policies :: Prelude.Maybe [ManagedPolicyDetail],
    -- | A flag that indicates whether there are more items to return. If your
    -- results were truncated, you can make a subsequent pagination request
    -- using the @Marker@ request parameter to retrieve more items. Note that
    -- IAM might return fewer than the @MaxItems@ number of results even when
    -- there are more results available. We recommend that you check
    -- @IsTruncated@ after every call to ensure that you receive all your
    -- results.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | A list containing information about IAM users.
    userDetailList :: Prelude.Maybe [UserDetail],
    -- | When @IsTruncated@ is @true@, this element is present and contains the
    -- value to use for the @Marker@ parameter in a subsequent pagination
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountAuthorizationDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleDetailList', 'getAccountAuthorizationDetailsResponse_roleDetailList' - A list containing information about IAM roles.
--
-- 'groupDetailList', 'getAccountAuthorizationDetailsResponse_groupDetailList' - A list containing information about IAM groups.
--
-- 'policies', 'getAccountAuthorizationDetailsResponse_policies' - A list containing information about managed policies.
--
-- 'isTruncated', 'getAccountAuthorizationDetailsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'userDetailList', 'getAccountAuthorizationDetailsResponse_userDetailList' - A list containing information about IAM users.
--
-- 'marker', 'getAccountAuthorizationDetailsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'getAccountAuthorizationDetailsResponse_httpStatus' - The response's http status code.
newGetAccountAuthorizationDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountAuthorizationDetailsResponse
newGetAccountAuthorizationDetailsResponse
  pHttpStatus_ =
    GetAccountAuthorizationDetailsResponse'
      { roleDetailList =
          Prelude.Nothing,
        groupDetailList = Prelude.Nothing,
        policies = Prelude.Nothing,
        isTruncated = Prelude.Nothing,
        userDetailList = Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list containing information about IAM roles.
getAccountAuthorizationDetailsResponse_roleDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [RoleDetail])
getAccountAuthorizationDetailsResponse_roleDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {roleDetailList} -> roleDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {roleDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list containing information about IAM groups.
getAccountAuthorizationDetailsResponse_groupDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [GroupDetail])
getAccountAuthorizationDetailsResponse_groupDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {groupDetailList} -> groupDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {groupDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list containing information about managed policies.
getAccountAuthorizationDetailsResponse_policies :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [ManagedPolicyDetail])
getAccountAuthorizationDetailsResponse_policies = Lens.lens (\GetAccountAuthorizationDetailsResponse' {policies} -> policies) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {policies = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
getAccountAuthorizationDetailsResponse_isTruncated :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe Prelude.Bool)
getAccountAuthorizationDetailsResponse_isTruncated = Lens.lens (\GetAccountAuthorizationDetailsResponse' {isTruncated} -> isTruncated) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {isTruncated = a} :: GetAccountAuthorizationDetailsResponse)

-- | A list containing information about IAM users.
getAccountAuthorizationDetailsResponse_userDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [UserDetail])
getAccountAuthorizationDetailsResponse_userDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {userDetailList} -> userDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {userDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
getAccountAuthorizationDetailsResponse_marker :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe Prelude.Text)
getAccountAuthorizationDetailsResponse_marker = Lens.lens (\GetAccountAuthorizationDetailsResponse' {marker} -> marker) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {marker = a} :: GetAccountAuthorizationDetailsResponse)

-- | The response's http status code.
getAccountAuthorizationDetailsResponse_httpStatus :: Lens.Lens' GetAccountAuthorizationDetailsResponse Prelude.Int
getAccountAuthorizationDetailsResponse_httpStatus = Lens.lens (\GetAccountAuthorizationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {httpStatus = a} :: GetAccountAuthorizationDetailsResponse)

instance
  Prelude.NFData
    GetAccountAuthorizationDetailsResponse
