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
-- Module      : Amazonka.IAM.ListInstanceProfileTags
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified IAM instance profile.
-- The returned list of tags is sorted by tag key. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
module Amazonka.IAM.ListInstanceProfileTags
  ( -- * Creating a Request
    ListInstanceProfileTags (..),
    newListInstanceProfileTags,

    -- * Request Lenses
    listInstanceProfileTags_marker,
    listInstanceProfileTags_maxItems,
    listInstanceProfileTags_instanceProfileName,

    -- * Destructuring the Response
    ListInstanceProfileTagsResponse (..),
    newListInstanceProfileTagsResponse,

    -- * Response Lenses
    listInstanceProfileTagsResponse_isTruncated,
    listInstanceProfileTagsResponse_marker,
    listInstanceProfileTagsResponse_httpStatus,
    listInstanceProfileTagsResponse_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceProfileTags' smart constructor.
data ListInstanceProfileTags = ListInstanceProfileTags'
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
    -- | The name of the IAM instance profile whose tags you want to see.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    instanceProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfileTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listInstanceProfileTags_marker' - Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
--
-- 'maxItems', 'listInstanceProfileTags_maxItems' - Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
--
-- 'instanceProfileName', 'listInstanceProfileTags_instanceProfileName' - The name of the IAM instance profile whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newListInstanceProfileTags ::
  -- | 'instanceProfileName'
  Prelude.Text ->
  ListInstanceProfileTags
newListInstanceProfileTags pInstanceProfileName_ =
  ListInstanceProfileTags'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      instanceProfileName = pInstanceProfileName_
    }

-- | Use this parameter only when paginating results and only after you
-- receive a response indicating that the results are truncated. Set it to
-- the value of the @Marker@ element in the response that you received to
-- indicate where the next call should start.
listInstanceProfileTags_marker :: Lens.Lens' ListInstanceProfileTags (Prelude.Maybe Prelude.Text)
listInstanceProfileTags_marker = Lens.lens (\ListInstanceProfileTags' {marker} -> marker) (\s@ListInstanceProfileTags' {} a -> s {marker = a} :: ListInstanceProfileTags)

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If additional items exist beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- If you do not include this parameter, the number of items defaults to
-- 100. Note that IAM might return fewer results, even when there are more
-- results available. In that case, the @IsTruncated@ response element
-- returns @true@, and @Marker@ contains a value to include in the
-- subsequent call that tells the service where to continue from.
listInstanceProfileTags_maxItems :: Lens.Lens' ListInstanceProfileTags (Prelude.Maybe Prelude.Natural)
listInstanceProfileTags_maxItems = Lens.lens (\ListInstanceProfileTags' {maxItems} -> maxItems) (\s@ListInstanceProfileTags' {} a -> s {maxItems = a} :: ListInstanceProfileTags)

-- | The name of the IAM instance profile whose tags you want to see.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
listInstanceProfileTags_instanceProfileName :: Lens.Lens' ListInstanceProfileTags Prelude.Text
listInstanceProfileTags_instanceProfileName = Lens.lens (\ListInstanceProfileTags' {instanceProfileName} -> instanceProfileName) (\s@ListInstanceProfileTags' {} a -> s {instanceProfileName = a} :: ListInstanceProfileTags)

instance Core.AWSRequest ListInstanceProfileTags where
  type
    AWSResponse ListInstanceProfileTags =
      ListInstanceProfileTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListInstanceProfileTagsResult"
      ( \s h x ->
          ListInstanceProfileTagsResponse'
            Prelude.<$> (x Data..@? "IsTruncated")
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "Tags"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable ListInstanceProfileTags where
  hashWithSalt _salt ListInstanceProfileTags' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` instanceProfileName

instance Prelude.NFData ListInstanceProfileTags where
  rnf ListInstanceProfileTags' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf instanceProfileName

instance Data.ToHeaders ListInstanceProfileTags where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListInstanceProfileTags where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInstanceProfileTags where
  toQuery ListInstanceProfileTags' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListInstanceProfileTags" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "InstanceProfileName" Data.=: instanceProfileName
      ]

-- | /See:/ 'newListInstanceProfileTagsResponse' smart constructor.
data ListInstanceProfileTagsResponse = ListInstanceProfileTagsResponse'
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
    -- | The list of tags that are currently attached to the IAM instance
    -- profile. Each tag consists of a key name and an associated value. If no
    -- tags are attached to the specified resource, the response contains an
    -- empty list.
    tags :: [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfileTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'listInstanceProfileTagsResponse_isTruncated' - A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
--
-- 'marker', 'listInstanceProfileTagsResponse_marker' - When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
--
-- 'httpStatus', 'listInstanceProfileTagsResponse_httpStatus' - The response's http status code.
--
-- 'tags', 'listInstanceProfileTagsResponse_tags' - The list of tags that are currently attached to the IAM instance
-- profile. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
newListInstanceProfileTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceProfileTagsResponse
newListInstanceProfileTagsResponse pHttpStatus_ =
  ListInstanceProfileTagsResponse'
    { isTruncated =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tags = Prelude.mempty
    }

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items. Note that
-- IAM might return fewer than the @MaxItems@ number of results even when
-- there are more results available. We recommend that you check
-- @IsTruncated@ after every call to ensure that you receive all your
-- results.
listInstanceProfileTagsResponse_isTruncated :: Lens.Lens' ListInstanceProfileTagsResponse (Prelude.Maybe Prelude.Bool)
listInstanceProfileTagsResponse_isTruncated = Lens.lens (\ListInstanceProfileTagsResponse' {isTruncated} -> isTruncated) (\s@ListInstanceProfileTagsResponse' {} a -> s {isTruncated = a} :: ListInstanceProfileTagsResponse)

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
listInstanceProfileTagsResponse_marker :: Lens.Lens' ListInstanceProfileTagsResponse (Prelude.Maybe Prelude.Text)
listInstanceProfileTagsResponse_marker = Lens.lens (\ListInstanceProfileTagsResponse' {marker} -> marker) (\s@ListInstanceProfileTagsResponse' {} a -> s {marker = a} :: ListInstanceProfileTagsResponse)

-- | The response's http status code.
listInstanceProfileTagsResponse_httpStatus :: Lens.Lens' ListInstanceProfileTagsResponse Prelude.Int
listInstanceProfileTagsResponse_httpStatus = Lens.lens (\ListInstanceProfileTagsResponse' {httpStatus} -> httpStatus) (\s@ListInstanceProfileTagsResponse' {} a -> s {httpStatus = a} :: ListInstanceProfileTagsResponse)

-- | The list of tags that are currently attached to the IAM instance
-- profile. Each tag consists of a key name and an associated value. If no
-- tags are attached to the specified resource, the response contains an
-- empty list.
listInstanceProfileTagsResponse_tags :: Lens.Lens' ListInstanceProfileTagsResponse [Tag]
listInstanceProfileTagsResponse_tags = Lens.lens (\ListInstanceProfileTagsResponse' {tags} -> tags) (\s@ListInstanceProfileTagsResponse' {} a -> s {tags = a} :: ListInstanceProfileTagsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListInstanceProfileTagsResponse
  where
  rnf ListInstanceProfileTagsResponse' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
