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
-- Module      : Network.AWS.LakeFormation.AddLFTagsToResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more tags to an existing resource.
module Network.AWS.LakeFormation.AddLFTagsToResource
  ( -- * Creating a Request
    AddLFTagsToResource (..),
    newAddLFTagsToResource,

    -- * Request Lenses
    addLFTagsToResource_catalogId,
    addLFTagsToResource_resource,
    addLFTagsToResource_lFTags,

    -- * Destructuring the Response
    AddLFTagsToResourceResponse (..),
    newAddLFTagsToResourceResponse,

    -- * Response Lenses
    addLFTagsToResourceResponse_failures,
    addLFTagsToResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.LakeFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddLFTagsToResource' smart constructor.
data AddLFTagsToResource = AddLFTagsToResource'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The resource to which to attach a tag.
    resource :: Resource,
    -- | The tags to attach to the resource.
    lFTags :: Prelude.NonEmpty LFTagPair
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddLFTagsToResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'addLFTagsToResource_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'resource', 'addLFTagsToResource_resource' - The resource to which to attach a tag.
--
-- 'lFTags', 'addLFTagsToResource_lFTags' - The tags to attach to the resource.
newAddLFTagsToResource ::
  -- | 'resource'
  Resource ->
  -- | 'lFTags'
  Prelude.NonEmpty LFTagPair ->
  AddLFTagsToResource
newAddLFTagsToResource pResource_ pLFTags_ =
  AddLFTagsToResource'
    { catalogId = Prelude.Nothing,
      resource = pResource_,
      lFTags = Lens.coerced Lens.# pLFTags_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
addLFTagsToResource_catalogId :: Lens.Lens' AddLFTagsToResource (Prelude.Maybe Prelude.Text)
addLFTagsToResource_catalogId = Lens.lens (\AddLFTagsToResource' {catalogId} -> catalogId) (\s@AddLFTagsToResource' {} a -> s {catalogId = a} :: AddLFTagsToResource)

-- | The resource to which to attach a tag.
addLFTagsToResource_resource :: Lens.Lens' AddLFTagsToResource Resource
addLFTagsToResource_resource = Lens.lens (\AddLFTagsToResource' {resource} -> resource) (\s@AddLFTagsToResource' {} a -> s {resource = a} :: AddLFTagsToResource)

-- | The tags to attach to the resource.
addLFTagsToResource_lFTags :: Lens.Lens' AddLFTagsToResource (Prelude.NonEmpty LFTagPair)
addLFTagsToResource_lFTags = Lens.lens (\AddLFTagsToResource' {lFTags} -> lFTags) (\s@AddLFTagsToResource' {} a -> s {lFTags = a} :: AddLFTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddLFTagsToResource where
  type
    AWSResponse AddLFTagsToResource =
      AddLFTagsToResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddLFTagsToResourceResponse'
            Prelude.<$> (x Core..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddLFTagsToResource

instance Prelude.NFData AddLFTagsToResource

instance Core.ToHeaders AddLFTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.AddLFTagsToResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddLFTagsToResource where
  toJSON AddLFTagsToResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("Resource" Core..= resource),
            Prelude.Just ("LFTags" Core..= lFTags)
          ]
      )

instance Core.ToPath AddLFTagsToResource where
  toPath = Prelude.const "/"

instance Core.ToQuery AddLFTagsToResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddLFTagsToResourceResponse' smart constructor.
data AddLFTagsToResourceResponse = AddLFTagsToResourceResponse'
  { -- | A list of failures to tag the resource.
    failures :: Prelude.Maybe [LFTagError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddLFTagsToResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'addLFTagsToResourceResponse_failures' - A list of failures to tag the resource.
--
-- 'httpStatus', 'addLFTagsToResourceResponse_httpStatus' - The response's http status code.
newAddLFTagsToResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddLFTagsToResourceResponse
newAddLFTagsToResourceResponse pHttpStatus_ =
  AddLFTagsToResourceResponse'
    { failures =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of failures to tag the resource.
addLFTagsToResourceResponse_failures :: Lens.Lens' AddLFTagsToResourceResponse (Prelude.Maybe [LFTagError])
addLFTagsToResourceResponse_failures = Lens.lens (\AddLFTagsToResourceResponse' {failures} -> failures) (\s@AddLFTagsToResourceResponse' {} a -> s {failures = a} :: AddLFTagsToResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
addLFTagsToResourceResponse_httpStatus :: Lens.Lens' AddLFTagsToResourceResponse Prelude.Int
addLFTagsToResourceResponse_httpStatus = Lens.lens (\AddLFTagsToResourceResponse' {httpStatus} -> httpStatus) (\s@AddLFTagsToResourceResponse' {} a -> s {httpStatus = a} :: AddLFTagsToResourceResponse)

instance Prelude.NFData AddLFTagsToResourceResponse
