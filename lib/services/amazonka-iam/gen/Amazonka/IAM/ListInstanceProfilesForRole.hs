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
-- Module      : Amazonka.IAM.ListInstanceProfilesForRole
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified associated IAM role.
-- If there are none, the operation returns an empty list. For more
-- information about instance profiles, go to
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Amazonka.IAM.ListInstanceProfilesForRole
  ( -- * Creating a Request
    ListInstanceProfilesForRole (..),
    newListInstanceProfilesForRole,

    -- * Request Lenses
    listInstanceProfilesForRole_marker,
    listInstanceProfilesForRole_maxItems,
    listInstanceProfilesForRole_roleName,

    -- * Destructuring the Response
    ListInstanceProfilesForRoleResponse (..),
    newListInstanceProfilesForRoleResponse,

    -- * Response Lenses
    listInstanceProfilesForRoleResponse_isTruncated,
    listInstanceProfilesForRoleResponse_marker,
    listInstanceProfilesForRoleResponse_httpStatus,
    listInstanceProfilesForRoleResponse_instanceProfiles,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceProfilesForRole' smart constructor.
data ListInstanceProfilesForRole = ListInstanceProfilesForRole'
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
    -- | The name of the role to list instance profiles for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    roleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfilesForRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceProfilesForRole_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listInstanceProfilesForRole_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'roleName', 'listInstanceProfilesForRole_roleName' - The name of the role to list instance profiles for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListInstanceProfilesForRole ::
  -- | 'roleName'
  Prelude.Text ->
  ListInstanceProfilesForRole
newListInstanceProfilesForRole pRoleName_ =
  ListInstanceProfilesForRole'
    { marker =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      roleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listInstanceProfilesForRole_marker :: Lens.Lens' ListInstanceProfilesForRole (Prelude.Maybe Prelude.Text)
listInstanceProfilesForRole_marker = Lens.lens (\ListInstanceProfilesForRole' {marker} -> marker) (\s@ListInstanceProfilesForRole' {} a -> s {marker = a} :: ListInstanceProfilesForRole)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listInstanceProfilesForRole_maxItems :: Lens.Lens' ListInstanceProfilesForRole (Prelude.Maybe Prelude.Natural)
listInstanceProfilesForRole_maxItems = Lens.lens (\ListInstanceProfilesForRole' {maxItems} -> maxItems) (\s@ListInstanceProfilesForRole' {} a -> s {maxItems = a} :: ListInstanceProfilesForRole)

-- | The name of the role to list instance profiles for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listInstanceProfilesForRole_roleName :: Lens.Lens' ListInstanceProfilesForRole Prelude.Text
listInstanceProfilesForRole_roleName = Lens.lens (\ListInstanceProfilesForRole' {roleName} -> roleName) (\s@ListInstanceProfilesForRole' {} a -> s {roleName = a} :: ListInstanceProfilesForRole)

instance Core.AWSPager ListInstanceProfilesForRole where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesForRoleResponse_isTruncated
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listInstanceProfilesForRoleResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listInstanceProfilesForRole_marker
          Lens..~ rs
          Lens.^? listInstanceProfilesForRoleResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceProfilesForRole where
  type
    AWSResponse ListInstanceProfilesForRole =
      ListInstanceProfilesForRoleResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListInstanceProfilesForRoleResult"
      ( \s h x ->
          ListInstanceProfilesForRoleResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "InstanceProfiles"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListInstanceProfilesForRole where
  hashWithSalt _salt ListInstanceProfilesForRole' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` roleName

instance Prelude.NFData ListInstanceProfilesForRole where
  rnf ListInstanceProfilesForRole' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf roleName

instance Data.ToHeaders ListInstanceProfilesForRole where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListInstanceProfilesForRole where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInstanceProfilesForRole where
  toQuery ListInstanceProfilesForRole' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ListInstanceProfilesForRole" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "RoleName" Data.=: roleName
      ]

-- | Contains the response to a successful ListInstanceProfilesForRole
-- request.
--
-- /See:/ 'newListInstanceProfilesForRoleResponse' smart constructor.
data ListInstanceProfilesForRoleResponse = ListInstanceProfilesForRoleResponse'
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
    -- | A list of instance profiles.
    instanceProfiles :: [InstanceProfile]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfilesForRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listInstanceProfilesForRoleResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listInstanceProfilesForRoleResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listInstanceProfilesForRoleResponse_httpStatus' - The response's http status code.
--
-- 'instanceProfiles', 'listInstanceProfilesForRoleResponse_instanceProfiles' - A list of instance profiles.
newListInstanceProfilesForRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceProfilesForRoleResponse
newListInstanceProfilesForRoleResponse pHttpStatus_ =
  ListInstanceProfilesForRoleResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      instanceProfiles = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listInstanceProfilesForRoleResponse_isTruncated :: Lens.Lens' ListInstanceProfilesForRoleResponse (Prelude.Maybe Prelude.Bool)
listInstanceProfilesForRoleResponse_isTruncated = Lens.lens (\ListInstanceProfilesForRoleResponse' {isTruncated} -> isTruncated) (\s@ListInstanceProfilesForRoleResponse' {} a -> s {isTruncated = a} :: ListInstanceProfilesForRoleResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listInstanceProfilesForRoleResponse_marker :: Lens.Lens' ListInstanceProfilesForRoleResponse (Prelude.Maybe Prelude.Text)
listInstanceProfilesForRoleResponse_marker = Lens.lens (\ListInstanceProfilesForRoleResponse' {marker} -> marker) (\s@ListInstanceProfilesForRoleResponse' {} a -> s {marker = a} :: ListInstanceProfilesForRoleResponse)

-- | The response's http status code.
listInstanceProfilesForRoleResponse_httpStatus :: Lens.Lens' ListInstanceProfilesForRoleResponse Prelude.Int
listInstanceProfilesForRoleResponse_httpStatus = Lens.lens (\ListInstanceProfilesForRoleResponse' {httpStatus} -> httpStatus) (\s@ListInstanceProfilesForRoleResponse' {} a -> s {httpStatus = a} :: ListInstanceProfilesForRoleResponse)

-- | A list of instance profiles.
listInstanceProfilesForRoleResponse_instanceProfiles :: Lens.Lens' ListInstanceProfilesForRoleResponse [InstanceProfile]
listInstanceProfilesForRoleResponse_instanceProfiles = Lens.lens (\ListInstanceProfilesForRoleResponse' {instanceProfiles} -> instanceProfiles) (\s@ListInstanceProfilesForRoleResponse' {} a -> s {instanceProfiles = a} :: ListInstanceProfilesForRoleResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListInstanceProfilesForRoleResponse
  where
  rnf ListInstanceProfilesForRoleResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf instanceProfiles
