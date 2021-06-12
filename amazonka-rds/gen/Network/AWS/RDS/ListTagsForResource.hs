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
-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags on an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>
-- in the /Amazon RDS User Guide/.
module Network.AWS.RDS.ListTagsForResource
  ( -- * Creating a Request
    ListTagsForResource (..),
    newListTagsForResource,

    -- * Request Lenses
    listTagsForResource_filters,
    listTagsForResource_resourceName,

    -- * Destructuring the Response
    ListTagsForResourceResponse (..),
    newListTagsForResourceResponse,

    -- * Response Lenses
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | The Amazon RDS resource with tags to be listed. This value is an Amazon
    -- Resource Name (ARN). For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    resourceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTagsForResource_filters' - This parameter isn\'t currently supported.
--
-- 'resourceName', 'listTagsForResource_resourceName' - The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon RDS User Guide/.
newListTagsForResource ::
  -- | 'resourceName'
  Core.Text ->
  ListTagsForResource
newListTagsForResource pResourceName_ =
  ListTagsForResource'
    { filters = Core.Nothing,
      resourceName = pResourceName_
    }

-- | This parameter isn\'t currently supported.
listTagsForResource_filters :: Lens.Lens' ListTagsForResource (Core.Maybe [Filter])
listTagsForResource_filters = Lens.lens (\ListTagsForResource' {filters} -> filters) (\s@ListTagsForResource' {} a -> s {filters = a} :: ListTagsForResource) Core.. Lens.mapping Lens._Coerce

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
-- in the /Amazon RDS User Guide/.
listTagsForResource_resourceName :: Lens.Lens' ListTagsForResource Core.Text
listTagsForResource_resourceName = Lens.lens (\ListTagsForResource' {resourceName} -> resourceName) (\s@ListTagsForResource' {} a -> s {resourceName = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Core.<$> ( x Core..@? "TagList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Tag")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTagsForResource

instance Core.NFData ListTagsForResource

instance Core.ToHeaders ListTagsForResource where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListTagsForResource where
  toPath = Core.const "/"

instance Core.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ListTagsForResource" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "ResourceName" Core.=: resourceName
      ]

-- |
--
-- /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | List of tags returned by the ListTagsForResource operation.
    tagList :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTagsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagList', 'listTagsForResourceResponse_tagList' - List of tags returned by the ListTagsForResource operation.
--
-- 'httpStatus', 'listTagsForResourceResponse_httpStatus' - The response's http status code.
newListTagsForResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { tagList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of tags returned by the ListTagsForResource operation.
listTagsForResourceResponse_tagList :: Lens.Lens' ListTagsForResourceResponse (Core.Maybe [Tag])
listTagsForResourceResponse_tagList = Lens.lens (\ListTagsForResourceResponse' {tagList} -> tagList) (\s@ListTagsForResourceResponse' {} a -> s {tagList = a} :: ListTagsForResourceResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Core.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Core.NFData ListTagsForResourceResponse
