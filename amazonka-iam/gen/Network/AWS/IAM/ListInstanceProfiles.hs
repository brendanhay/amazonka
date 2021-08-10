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
-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified path prefix. If
-- there are none, the operation returns an empty list. For more
-- information about instance profiles, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About instance profiles>.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for an instance profile, see GetInstanceProfile.
--
-- You can paginate the results using the @MaxItems@ and @Marker@
-- parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfiles
  ( -- * Creating a Request
    ListInstanceProfiles (..),
    newListInstanceProfiles,

    -- * Request Lenses
    listInstanceProfiles_pathPrefix,
    listInstanceProfiles_maxItems,
    listInstanceProfiles_marker,

    -- * Destructuring the Response
    ListInstanceProfilesResponse (..),
    newListInstanceProfilesResponse,

    -- * Response Lenses
    listInstanceProfilesResponse_isTruncated,
    listInstanceProfilesResponse_marker,
    listInstanceProfilesResponse_httpStatus,
    listInstanceProfilesResponse_instanceProfiles,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { -- | The path prefix for filtering the results. For example, the prefix
    -- @\/application_abc\/component_xyz\/@ gets all instance profiles whose
    -- path starts with @\/application_abc\/component_xyz\/@.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/), listing all instance profiles. This parameter allows
    -- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
    -- of characters consisting of either a forward slash (\/) by itself or a
    -- string that must begin and end with forward slashes. In addition, it can
    -- contain any ASCII character from the ! (@\\u0021@) through the DEL
    -- character (@\\u007F@), including most punctuation characters, digits,
    -- and upper and lowercased letters.
    pathPrefix :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListInstanceProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPrefix', 'listInstanceProfiles_pathPrefix' - The path prefix for filtering the results. For example, the prefix
-- @\/application_abc\/component_xyz\/@ gets all instance profiles whose
-- path starts with @\/application_abc\/component_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all instance profiles. This parameter allows
-- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
-- of characters consisting of either a forward slash (\/) by itself or a
-- string that must begin and end with forward slashes. In addition, it can
-- contain any ASCII character from the ! (@\\u0021@) through the DEL
-- character (@\\u007F@), including most punctuation characters, digits,
-- and upper and lowercased letters.
--
-- 'maxItems', 'listInstanceProfiles_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'marker', 'listInstanceProfiles_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
newListInstanceProfiles ::
  ListInstanceProfiles
newListInstanceProfiles =
  ListInstanceProfiles'
    { pathPrefix = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix
-- @\/application_abc\/component_xyz\/@ gets all instance profiles whose
-- path starts with @\/application_abc\/component_xyz\/@.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/), listing all instance profiles. This parameter allows
-- (through its <http://wikipedia.org/wiki/regex regex pattern>) a string
-- of characters consisting of either a forward slash (\/) by itself or a
-- string that must begin and end with forward slashes. In addition, it can
-- contain any ASCII character from the ! (@\\u0021@) through the DEL
-- character (@\\u007F@), including most punctuation characters, digits,
-- and upper and lowercased letters.
listInstanceProfiles_pathPrefix :: Lens.Lens' ListInstanceProfiles (Prelude.Maybe Prelude.Text)
listInstanceProfiles_pathPrefix = Lens.lens (\ListInstanceProfiles' {pathPrefix} -> pathPrefix) (\s@ListInstanceProfiles' {} a -> s {pathPrefix = a} :: ListInstanceProfiles)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listInstanceProfiles_maxItems :: Lens.Lens' ListInstanceProfiles (Prelude.Maybe Prelude.Natural)
listInstanceProfiles_maxItems = Lens.lens (\ListInstanceProfiles' {maxItems} -> maxItems) (\s@ListInstanceProfiles' {} a -> s {maxItems = a} :: ListInstanceProfiles)

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listInstanceProfiles_marker :: Lens.Lens' ListInstanceProfiles (Prelude.Maybe Prelude.Text)
listInstanceProfiles_marker = Lens.lens (\ListInstanceProfiles' {marker} -> marker) (\s@ListInstanceProfiles' {} a -> s {marker = a} :: ListInstanceProfiles)

instance Core.AWSPager ListInstanceProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesResponse_isTruncated
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listInstanceProfilesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceProfiles_marker
          Lens..~ rs
          Lens.^? listInstanceProfilesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceProfiles where
  type
    AWSResponse ListInstanceProfiles =
      ListInstanceProfilesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListInstanceProfilesResult"
      ( \s h x ->
          ListInstanceProfilesResponse'
            Prelude.<$> (x Core..@? "IsTruncated")
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..@? "InstanceProfiles"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListInstanceProfiles

instance Prelude.NFData ListInstanceProfiles

instance Core.ToHeaders ListInstanceProfiles where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListInstanceProfiles where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInstanceProfiles where
  toQuery ListInstanceProfiles' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListInstanceProfiles" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "PathPrefix" Core.=: pathPrefix,
        "MaxItems" Core.=: maxItems,
        "Marker" Core.=: marker
      ]

-- | Contains the response to a successful ListInstanceProfiles request.
--
-- /See:/ 'newListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
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
-- Create a value of 'ListInstanceProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listInstanceProfilesResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listInstanceProfilesResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listInstanceProfilesResponse_httpStatus' - The response's http status code.
--
-- 'instanceProfiles', 'listInstanceProfilesResponse_instanceProfiles' - A list of instance profiles.
newListInstanceProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceProfilesResponse
newListInstanceProfilesResponse pHttpStatus_ =
  ListInstanceProfilesResponse'
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
listInstanceProfilesResponse_isTruncated :: Lens.Lens' ListInstanceProfilesResponse (Prelude.Maybe Prelude.Bool)
listInstanceProfilesResponse_isTruncated = Lens.lens (\ListInstanceProfilesResponse' {isTruncated} -> isTruncated) (\s@ListInstanceProfilesResponse' {} a -> s {isTruncated = a} :: ListInstanceProfilesResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listInstanceProfilesResponse_marker :: Lens.Lens' ListInstanceProfilesResponse (Prelude.Maybe Prelude.Text)
listInstanceProfilesResponse_marker = Lens.lens (\ListInstanceProfilesResponse' {marker} -> marker) (\s@ListInstanceProfilesResponse' {} a -> s {marker = a} :: ListInstanceProfilesResponse)

-- | The response's http status code.
listInstanceProfilesResponse_httpStatus :: Lens.Lens' ListInstanceProfilesResponse Prelude.Int
listInstanceProfilesResponse_httpStatus = Lens.lens (\ListInstanceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListInstanceProfilesResponse' {} a -> s {httpStatus = a} :: ListInstanceProfilesResponse)

-- | A list of instance profiles.
listInstanceProfilesResponse_instanceProfiles :: Lens.Lens' ListInstanceProfilesResponse [InstanceProfile]
listInstanceProfilesResponse_instanceProfiles = Lens.lens (\ListInstanceProfilesResponse' {instanceProfiles} -> instanceProfiles) (\s@ListInstanceProfilesResponse' {} a -> s {instanceProfiles = a} :: ListInstanceProfilesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListInstanceProfilesResponse
