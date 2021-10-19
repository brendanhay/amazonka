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
-- Module      : Network.AWS.MachineLearning.DescribeTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the tags for your Amazon ML object.
module Network.AWS.MachineLearning.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_resourceId,
    describeTags_resourceType,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_resourceId,
    describeTagsResponse_resourceType,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MachineLearning.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The ID of the ML object. For example, @exampleModelId@.
    resourceId :: Prelude.Text,
    -- | The type of the ML object.
    resourceType :: TaggableResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeTags_resourceId' - The ID of the ML object. For example, @exampleModelId@.
--
-- 'resourceType', 'describeTags_resourceType' - The type of the ML object.
newDescribeTags ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  TaggableResourceType ->
  DescribeTags
newDescribeTags pResourceId_ pResourceType_ =
  DescribeTags'
    { resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The ID of the ML object. For example, @exampleModelId@.
describeTags_resourceId :: Lens.Lens' DescribeTags Prelude.Text
describeTags_resourceId = Lens.lens (\DescribeTags' {resourceId} -> resourceId) (\s@DescribeTags' {} a -> s {resourceId = a} :: DescribeTags)

-- | The type of the ML object.
describeTags_resourceType :: Lens.Lens' DescribeTags TaggableResourceType
describeTags_resourceType = Lens.lens (\DescribeTags' {resourceType} -> resourceType) (\s@DescribeTags' {} a -> s {resourceType = a} :: DescribeTags)

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Core..?> "ResourceId")
            Prelude.<*> (x Core..?> "ResourceType")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags

instance Prelude.NFData DescribeTags

instance Core.ToHeaders DescribeTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonML_20141212.DescribeTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Core..= resourceId),
            Prelude.Just ("ResourceType" Core..= resourceType)
          ]
      )

instance Core.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTags where
  toQuery = Prelude.const Prelude.mempty

-- | Amazon ML returns the following elements.
--
-- /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The ID of the tagged ML object.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the tagged ML object.
    resourceType :: Prelude.Maybe TaggableResourceType,
    -- | A list of tags associated with the ML object.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeTagsResponse_resourceId' - The ID of the tagged ML object.
--
-- 'resourceType', 'describeTagsResponse_resourceType' - The type of the tagged ML object.
--
-- 'tags', 'describeTagsResponse_tags' - A list of tags associated with the ML object.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the tagged ML object.
describeTagsResponse_resourceId :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe Prelude.Text)
describeTagsResponse_resourceId = Lens.lens (\DescribeTagsResponse' {resourceId} -> resourceId) (\s@DescribeTagsResponse' {} a -> s {resourceId = a} :: DescribeTagsResponse)

-- | The type of the tagged ML object.
describeTagsResponse_resourceType :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe TaggableResourceType)
describeTagsResponse_resourceType = Lens.lens (\DescribeTagsResponse' {resourceType} -> resourceType) (\s@DescribeTagsResponse' {} a -> s {resourceType = a} :: DescribeTagsResponse)

-- | A list of tags associated with the ML object.
describeTagsResponse_tags :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [Tag])
describeTagsResponse_tags = Lens.lens (\DescribeTagsResponse' {tags} -> tags) (\s@DescribeTagsResponse' {} a -> s {tags = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse
