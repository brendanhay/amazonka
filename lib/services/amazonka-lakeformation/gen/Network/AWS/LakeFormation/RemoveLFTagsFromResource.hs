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
-- Module      : Network.AWS.LakeFormation.RemoveLFTagsFromResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a tag from the resource. Only database, table, or
-- tableWithColumns resource are allowed. To tag columns, use the column
-- inclusion list in @tableWithColumns@ to specify column input.
module Network.AWS.LakeFormation.RemoveLFTagsFromResource
  ( -- * Creating a Request
    RemoveLFTagsFromResource (..),
    newRemoveLFTagsFromResource,

    -- * Request Lenses
    removeLFTagsFromResource_catalogId,
    removeLFTagsFromResource_resource,
    removeLFTagsFromResource_lFTags,

    -- * Destructuring the Response
    RemoveLFTagsFromResourceResponse (..),
    newRemoveLFTagsFromResourceResponse,

    -- * Response Lenses
    removeLFTagsFromResourceResponse_failures,
    removeLFTagsFromResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.LakeFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveLFTagsFromResource' smart constructor.
data RemoveLFTagsFromResource = RemoveLFTagsFromResource'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The resource where you want to remove a tag.
    resource :: Resource,
    -- | The tags to be removed from the resource.
    lFTags :: Prelude.NonEmpty LFTagPair
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveLFTagsFromResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'removeLFTagsFromResource_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'resource', 'removeLFTagsFromResource_resource' - The resource where you want to remove a tag.
--
-- 'lFTags', 'removeLFTagsFromResource_lFTags' - The tags to be removed from the resource.
newRemoveLFTagsFromResource ::
  -- | 'resource'
  Resource ->
  -- | 'lFTags'
  Prelude.NonEmpty LFTagPair ->
  RemoveLFTagsFromResource
newRemoveLFTagsFromResource pResource_ pLFTags_ =
  RemoveLFTagsFromResource'
    { catalogId =
        Prelude.Nothing,
      resource = pResource_,
      lFTags = Lens.coerced Lens.# pLFTags_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
removeLFTagsFromResource_catalogId :: Lens.Lens' RemoveLFTagsFromResource (Prelude.Maybe Prelude.Text)
removeLFTagsFromResource_catalogId = Lens.lens (\RemoveLFTagsFromResource' {catalogId} -> catalogId) (\s@RemoveLFTagsFromResource' {} a -> s {catalogId = a} :: RemoveLFTagsFromResource)

-- | The resource where you want to remove a tag.
removeLFTagsFromResource_resource :: Lens.Lens' RemoveLFTagsFromResource Resource
removeLFTagsFromResource_resource = Lens.lens (\RemoveLFTagsFromResource' {resource} -> resource) (\s@RemoveLFTagsFromResource' {} a -> s {resource = a} :: RemoveLFTagsFromResource)

-- | The tags to be removed from the resource.
removeLFTagsFromResource_lFTags :: Lens.Lens' RemoveLFTagsFromResource (Prelude.NonEmpty LFTagPair)
removeLFTagsFromResource_lFTags = Lens.lens (\RemoveLFTagsFromResource' {lFTags} -> lFTags) (\s@RemoveLFTagsFromResource' {} a -> s {lFTags = a} :: RemoveLFTagsFromResource) Prelude.. Lens.coerced

instance Core.AWSRequest RemoveLFTagsFromResource where
  type
    AWSResponse RemoveLFTagsFromResource =
      RemoveLFTagsFromResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveLFTagsFromResourceResponse'
            Prelude.<$> (x Core..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveLFTagsFromResource

instance Prelude.NFData RemoveLFTagsFromResource

instance Core.ToHeaders RemoveLFTagsFromResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.RemoveLFTagsFromResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RemoveLFTagsFromResource where
  toJSON RemoveLFTagsFromResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("Resource" Core..= resource),
            Prelude.Just ("LFTags" Core..= lFTags)
          ]
      )

instance Core.ToPath RemoveLFTagsFromResource where
  toPath = Prelude.const "/"

instance Core.ToQuery RemoveLFTagsFromResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveLFTagsFromResourceResponse' smart constructor.
data RemoveLFTagsFromResourceResponse = RemoveLFTagsFromResourceResponse'
  { -- | A list of failures to untag a resource.
    failures :: Prelude.Maybe [LFTagError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveLFTagsFromResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'removeLFTagsFromResourceResponse_failures' - A list of failures to untag a resource.
--
-- 'httpStatus', 'removeLFTagsFromResourceResponse_httpStatus' - The response's http status code.
newRemoveLFTagsFromResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveLFTagsFromResourceResponse
newRemoveLFTagsFromResourceResponse pHttpStatus_ =
  RemoveLFTagsFromResourceResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of failures to untag a resource.
removeLFTagsFromResourceResponse_failures :: Lens.Lens' RemoveLFTagsFromResourceResponse (Prelude.Maybe [LFTagError])
removeLFTagsFromResourceResponse_failures = Lens.lens (\RemoveLFTagsFromResourceResponse' {failures} -> failures) (\s@RemoveLFTagsFromResourceResponse' {} a -> s {failures = a} :: RemoveLFTagsFromResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
removeLFTagsFromResourceResponse_httpStatus :: Lens.Lens' RemoveLFTagsFromResourceResponse Prelude.Int
removeLFTagsFromResourceResponse_httpStatus = Lens.lens (\RemoveLFTagsFromResourceResponse' {httpStatus} -> httpStatus) (\s@RemoveLFTagsFromResourceResponse' {} a -> s {httpStatus = a} :: RemoveLFTagsFromResourceResponse)

instance
  Prelude.NFData
    RemoveLFTagsFromResourceResponse
