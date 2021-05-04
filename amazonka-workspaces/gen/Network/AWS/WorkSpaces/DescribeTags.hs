{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_resourceId,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_tagList,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The identifier of the WorkSpaces resource. The supported resource types
    -- are WorkSpaces, registered directories, images, custom bundles, IP
    -- access control groups, and connection aliases.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeTags_resourceId' - The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
newDescribeTags ::
  -- | 'resourceId'
  Prelude.Text ->
  DescribeTags
newDescribeTags pResourceId_ =
  DescribeTags' {resourceId = pResourceId_}

-- | The identifier of the WorkSpaces resource. The supported resource types
-- are WorkSpaces, registered directories, images, custom bundles, IP
-- access control groups, and connection aliases.
describeTags_resourceId :: Lens.Lens' DescribeTags Prelude.Text
describeTags_resourceId = Lens.lens (\DescribeTags' {resourceId} -> resourceId) (\s@DescribeTags' {} a -> s {resourceId = a} :: DescribeTags)

instance Prelude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Prelude..?> "TagList" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags

instance Prelude.NFData DescribeTags

instance Prelude.ToHeaders DescribeTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DescribeTags" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceId" Prelude..= resourceId)]
      )

instance Prelude.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The tags.
    tagList :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'describeTagsResponse_tagList' - The tags.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { tagList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags.
describeTagsResponse_tagList :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [Tag])
describeTagsResponse_tagList = Lens.lens (\DescribeTagsResponse' {tagList} -> tagList) (\s@DescribeTagsResponse' {} a -> s {tagList = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse
