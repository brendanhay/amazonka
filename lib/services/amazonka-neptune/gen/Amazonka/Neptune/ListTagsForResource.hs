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
-- Module      : Amazonka.Neptune.ListTagsForResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags on an Amazon Neptune resource.
module Amazonka.Neptune.ListTagsForResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The Amazon Neptune resource with tags to be listed. This value is an
    -- Amazon Resource Name (ARN). For information about creating an ARN, see
    -- <https://docs.aws.amazon.com/neptune/latest/UserGuide/tagging.ARN.html#tagging.ARN.Constructing Constructing an Amazon Resource Name (ARN)>.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listTagsForResource_filters' - This parameter is not currently supported.
--
-- 'resourceName', 'listTagsForResource_resourceName' - The Amazon Neptune resource with tags to be listed. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/neptune/latest/UserGuide/tagging.ARN.html#tagging.ARN.Constructing Constructing an Amazon Resource Name (ARN)>.
newListTagsForResource ::
  -- | 'resourceName'
  Prelude.Text ->
  ListTagsForResource
newListTagsForResource pResourceName_ =
  ListTagsForResource'
    { filters = Prelude.Nothing,
      resourceName = pResourceName_
    }

-- | This parameter is not currently supported.
listTagsForResource_filters :: Lens.Lens' ListTagsForResource (Prelude.Maybe [Filter])
listTagsForResource_filters = Lens.lens (\ListTagsForResource' {filters} -> filters) (\s@ListTagsForResource' {} a -> s {filters = a} :: ListTagsForResource) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Neptune resource with tags to be listed. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <https://docs.aws.amazon.com/neptune/latest/UserGuide/tagging.ARN.html#tagging.ARN.Constructing Constructing an Amazon Resource Name (ARN)>.
listTagsForResource_resourceName :: Lens.Lens' ListTagsForResource Prelude.Text
listTagsForResource_resourceName = Lens.lens (\ListTagsForResource' {resourceName} -> resourceName) (\s@ListTagsForResource' {} a -> s {resourceName = a} :: ListTagsForResource)

instance Core.AWSRequest ListTagsForResource where
  type
    AWSResponse ListTagsForResource =
      ListTagsForResourceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListTagsForResourceResult"
      ( \s h x ->
          ListTagsForResourceResponse'
            Prelude.<$> ( x Data..@? "TagList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Tag")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTagsForResource where
  hashWithSalt _salt ListTagsForResource' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData ListTagsForResource where
  rnf ListTagsForResource' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf resourceName

instance Data.ToHeaders ListTagsForResource where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListTagsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListTagsForResource" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "ResourceName" Data.=: resourceName
      ]

-- | /See:/ 'newListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | List of tags returned by the ListTagsForResource operation.
    tagList :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTagsForResourceResponse
newListTagsForResourceResponse pHttpStatus_ =
  ListTagsForResourceResponse'
    { tagList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of tags returned by the ListTagsForResource operation.
listTagsForResourceResponse_tagList :: Lens.Lens' ListTagsForResourceResponse (Prelude.Maybe [Tag])
listTagsForResourceResponse_tagList = Lens.lens (\ListTagsForResourceResponse' {tagList} -> tagList) (\s@ListTagsForResourceResponse' {} a -> s {tagList = a} :: ListTagsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTagsForResourceResponse_httpStatus :: Lens.Lens' ListTagsForResourceResponse Prelude.Int
listTagsForResourceResponse_httpStatus = Lens.lens (\ListTagsForResourceResponse' {httpStatus} -> httpStatus) (\s@ListTagsForResourceResponse' {} a -> s {httpStatus = a} :: ListTagsForResourceResponse)

instance Prelude.NFData ListTagsForResourceResponse where
  rnf ListTagsForResourceResponse' {..} =
    Prelude.rnf tagList `Prelude.seq`
      Prelude.rnf httpStatus
