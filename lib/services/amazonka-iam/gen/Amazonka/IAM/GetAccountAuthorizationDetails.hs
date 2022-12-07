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
-- Module      : Amazonka.IAM.GetAccountAuthorizationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all IAM users, groups, roles, and policies
-- in your Amazon Web Services account, including their relationships to
-- one another. Use this operation to obtain a snapshot of the
-- configuration of IAM permissions (users, groups, roles, and policies) in
-- your account.
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
module Amazonka.IAM.GetAccountAuthorizationDetails
  ( -- * Creating a Request
    GetAccountAuthorizationDetails (..),
    newGetAccountAuthorizationDetails,

    -- * Request Lenses
    getAccountAuthorizationDetails_marker,
    getAccountAuthorizationDetails_maxItems,
    getAccountAuthorizationDetails_filter,

    -- * Destructuring the Response
    GetAccountAuthorizationDetailsResponse (..),
    newGetAccountAuthorizationDetailsResponse,

    -- * Response Lenses
    getAccountAuthorizationDetailsResponse_marker,
    getAccountAuthorizationDetailsResponse_isTruncated,
    getAccountAuthorizationDetailsResponse_policies,
    getAccountAuthorizationDetailsResponse_roleDetailList,
    getAccountAuthorizationDetailsResponse_userDetailList,
    getAccountAuthorizationDetailsResponse_groupDetailList,
    getAccountAuthorizationDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountAuthorizationDetails' smart constructor.
data GetAccountAuthorizationDetails = GetAccountAuthorizationDetails'
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
    -- | A list of entity types used to filter the results. Only the entities
    -- that match the types you specify are included in the output. Use the
    -- value @LocalManagedPolicy@ to include customer managed policies.
    --
    -- The format for this parameter is a comma-separated (if more than one)
    -- list of strings. Each string value in the list must be one of the valid
    -- values listed below.
    filter' :: Prelude.Maybe [EntityType]
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
-- 'marker', 'getAccountAuthorizationDetails_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
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
-- 'filter'', 'getAccountAuthorizationDetails_filter' - A list of entity types used to filter the results. Only the entities
-- that match the types you specify are included in the output. Use the
-- value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one)
-- list of strings. Each string value in the list must be one of the valid
-- values listed below.
newGetAccountAuthorizationDetails ::
  GetAccountAuthorizationDetails
newGetAccountAuthorizationDetails =
  GetAccountAuthorizationDetails'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
getAccountAuthorizationDetails_marker :: Lens.Lens' GetAccountAuthorizationDetails (Prelude.Maybe Prelude.Text)
getAccountAuthorizationDetails_marker = Lens.lens (\GetAccountAuthorizationDetails' {marker} -> marker) (\s@GetAccountAuthorizationDetails' {} a -> s {marker = a} :: GetAccountAuthorizationDetails)

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

-- | A list of entity types used to filter the results. Only the entities
-- that match the types you specify are included in the output. Use the
-- value @LocalManagedPolicy@ to include customer managed policies.
--
-- The format for this parameter is a comma-separated (if more than one)
-- list of strings. Each string value in the list must be one of the valid
-- values listed below.
getAccountAuthorizationDetails_filter :: Lens.Lens' GetAccountAuthorizationDetails (Prelude.Maybe [EntityType])
getAccountAuthorizationDetails_filter = Lens.lens (\GetAccountAuthorizationDetails' {filter'} -> filter') (\s@GetAccountAuthorizationDetails' {} a -> s {filter' = a} :: GetAccountAuthorizationDetails) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAccountAuthorizationDetailsResult"
      ( \s h x ->
          GetAccountAuthorizationDetailsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> (x Data..@? "IsTruncated")
            Prelude.<*> ( x Data..@? "Policies" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "RoleDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "UserDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "GroupDetailList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAccountAuthorizationDetails
  where
  hashWithSalt
    _salt
    GetAccountAuthorizationDetails' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxItems
        `Prelude.hashWithSalt` filter'

instance
  Prelude.NFData
    GetAccountAuthorizationDetails
  where
  rnf GetAccountAuthorizationDetails' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf filter'

instance
  Data.ToHeaders
    GetAccountAuthorizationDetails
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccountAuthorizationDetails where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccountAuthorizationDetails where
  toQuery GetAccountAuthorizationDetails' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetAccountAuthorizationDetails" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "Filter"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> filter')
      ]

-- | Contains the response to a successful GetAccountAuthorizationDetails
-- request.
--
-- /See:/ 'newGetAccountAuthorizationDetailsResponse' smart constructor.
data GetAccountAuthorizationDetailsResponse = GetAccountAuthorizationDetailsResponse'
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
    -- | A list containing information about managed policies.
    policies :: Prelude.Maybe [ManagedPolicyDetail],
    -- | A list containing information about IAM roles.
    roleDetailList :: Prelude.Maybe [RoleDetail],
    -- | A list containing information about IAM users.
    userDetailList :: Prelude.Maybe [UserDetail],
    -- | A list containing information about IAM groups.
    groupDetailList :: Prelude.Maybe [GroupDetail],
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
-- 'marker', 'getAccountAuthorizationDetailsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'isTruncated', 'getAccountAuthorizationDetailsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'policies', 'getAccountAuthorizationDetailsResponse_policies' - A list containing information about managed policies.
--
-- 'roleDetailList', 'getAccountAuthorizationDetailsResponse_roleDetailList' - A list containing information about IAM roles.
--
-- 'userDetailList', 'getAccountAuthorizationDetailsResponse_userDetailList' - A list containing information about IAM users.
--
-- 'groupDetailList', 'getAccountAuthorizationDetailsResponse_groupDetailList' - A list containing information about IAM groups.
--
-- 'httpStatus', 'getAccountAuthorizationDetailsResponse_httpStatus' - The response's http status code.
newGetAccountAuthorizationDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountAuthorizationDetailsResponse
newGetAccountAuthorizationDetailsResponse
  pHttpStatus_ =
    GetAccountAuthorizationDetailsResponse'
      { marker =
          Prelude.Nothing,
        isTruncated = Prelude.Nothing,
        policies = Prelude.Nothing,
        roleDetailList = Prelude.Nothing,
        userDetailList = Prelude.Nothing,
        groupDetailList = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
getAccountAuthorizationDetailsResponse_marker :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe Prelude.Text)
getAccountAuthorizationDetailsResponse_marker = Lens.lens (\GetAccountAuthorizationDetailsResponse' {marker} -> marker) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {marker = a} :: GetAccountAuthorizationDetailsResponse)

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
getAccountAuthorizationDetailsResponse_isTruncated :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe Prelude.Bool)
getAccountAuthorizationDetailsResponse_isTruncated = Lens.lens (\GetAccountAuthorizationDetailsResponse' {isTruncated} -> isTruncated) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {isTruncated = a} :: GetAccountAuthorizationDetailsResponse)

-- | A list containing information about managed policies.
getAccountAuthorizationDetailsResponse_policies :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [ManagedPolicyDetail])
getAccountAuthorizationDetailsResponse_policies = Lens.lens (\GetAccountAuthorizationDetailsResponse' {policies} -> policies) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {policies = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list containing information about IAM roles.
getAccountAuthorizationDetailsResponse_roleDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [RoleDetail])
getAccountAuthorizationDetailsResponse_roleDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {roleDetailList} -> roleDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {roleDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list containing information about IAM users.
getAccountAuthorizationDetailsResponse_userDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [UserDetail])
getAccountAuthorizationDetailsResponse_userDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {userDetailList} -> userDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {userDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list containing information about IAM groups.
getAccountAuthorizationDetailsResponse_groupDetailList :: Lens.Lens' GetAccountAuthorizationDetailsResponse (Prelude.Maybe [GroupDetail])
getAccountAuthorizationDetailsResponse_groupDetailList = Lens.lens (\GetAccountAuthorizationDetailsResponse' {groupDetailList} -> groupDetailList) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {groupDetailList = a} :: GetAccountAuthorizationDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAccountAuthorizationDetailsResponse_httpStatus :: Lens.Lens' GetAccountAuthorizationDetailsResponse Prelude.Int
getAccountAuthorizationDetailsResponse_httpStatus = Lens.lens (\GetAccountAuthorizationDetailsResponse' {httpStatus} -> httpStatus) (\s@GetAccountAuthorizationDetailsResponse' {} a -> s {httpStatus = a} :: GetAccountAuthorizationDetailsResponse)

instance
  Prelude.NFData
    GetAccountAuthorizationDetailsResponse
  where
  rnf GetAccountAuthorizationDetailsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf roleDetailList
      `Prelude.seq` Prelude.rnf userDetailList
      `Prelude.seq` Prelude.rnf groupDetailList
      `Prelude.seq` Prelude.rnf httpStatus
