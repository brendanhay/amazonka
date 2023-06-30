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
-- Module      : Amazonka.LakeFormation.AddLFTagsToResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more LF-tags to an existing resource.
module Amazonka.LakeFormation.AddLFTagsToResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddLFTagsToResource' smart constructor.
data AddLFTagsToResource = AddLFTagsToResource'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The database, table, or column resource to which to attach an LF-tag.
    resource :: Resource,
    -- | The LF-tags to attach to the resource.
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
-- your Lake Formation environment.
--
-- 'resource', 'addLFTagsToResource_resource' - The database, table, or column resource to which to attach an LF-tag.
--
-- 'lFTags', 'addLFTagsToResource_lFTags' - The LF-tags to attach to the resource.
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
-- your Lake Formation environment.
addLFTagsToResource_catalogId :: Lens.Lens' AddLFTagsToResource (Prelude.Maybe Prelude.Text)
addLFTagsToResource_catalogId = Lens.lens (\AddLFTagsToResource' {catalogId} -> catalogId) (\s@AddLFTagsToResource' {} a -> s {catalogId = a} :: AddLFTagsToResource)

-- | The database, table, or column resource to which to attach an LF-tag.
addLFTagsToResource_resource :: Lens.Lens' AddLFTagsToResource Resource
addLFTagsToResource_resource = Lens.lens (\AddLFTagsToResource' {resource} -> resource) (\s@AddLFTagsToResource' {} a -> s {resource = a} :: AddLFTagsToResource)

-- | The LF-tags to attach to the resource.
addLFTagsToResource_lFTags :: Lens.Lens' AddLFTagsToResource (Prelude.NonEmpty LFTagPair)
addLFTagsToResource_lFTags = Lens.lens (\AddLFTagsToResource' {lFTags} -> lFTags) (\s@AddLFTagsToResource' {} a -> s {lFTags = a} :: AddLFTagsToResource) Prelude.. Lens.coerced

instance Core.AWSRequest AddLFTagsToResource where
  type
    AWSResponse AddLFTagsToResource =
      AddLFTagsToResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddLFTagsToResourceResponse'
            Prelude.<$> (x Data..?> "Failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddLFTagsToResource where
  hashWithSalt _salt AddLFTagsToResource' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` lFTags

instance Prelude.NFData AddLFTagsToResource where
  rnf AddLFTagsToResource' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf lFTags

instance Data.ToHeaders AddLFTagsToResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddLFTagsToResource where
  toJSON AddLFTagsToResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("Resource" Data..= resource),
            Prelude.Just ("LFTags" Data..= lFTags)
          ]
      )

instance Data.ToPath AddLFTagsToResource where
  toPath = Prelude.const "/AddLFTagsToResource"

instance Data.ToQuery AddLFTagsToResource where
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

instance Prelude.NFData AddLFTagsToResourceResponse where
  rnf AddLFTagsToResourceResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
