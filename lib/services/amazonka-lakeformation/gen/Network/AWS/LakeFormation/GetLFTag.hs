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
-- Module      : Network.AWS.LakeFormation.GetLFTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a tag definition.
module Network.AWS.LakeFormation.GetLFTag
  ( -- * Creating a Request
    GetLFTag (..),
    newGetLFTag,

    -- * Request Lenses
    getLFTag_catalogId,
    getLFTag_tagKey,

    -- * Destructuring the Response
    GetLFTagResponse (..),
    newGetLFTagResponse,

    -- * Response Lenses
    getLFTagResponse_tagValues,
    getLFTagResponse_catalogId,
    getLFTagResponse_tagKey,
    getLFTagResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.LakeFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLFTag' smart constructor.
data GetLFTag = GetLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the tag.
    tagKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getLFTag_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'tagKey', 'getLFTag_tagKey' - The key-name for the tag.
newGetLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  GetLFTag
newGetLFTag pTagKey_ =
  GetLFTag'
    { catalogId = Prelude.Nothing,
      tagKey = pTagKey_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
getLFTag_catalogId :: Lens.Lens' GetLFTag (Prelude.Maybe Prelude.Text)
getLFTag_catalogId = Lens.lens (\GetLFTag' {catalogId} -> catalogId) (\s@GetLFTag' {} a -> s {catalogId = a} :: GetLFTag)

-- | The key-name for the tag.
getLFTag_tagKey :: Lens.Lens' GetLFTag Prelude.Text
getLFTag_tagKey = Lens.lens (\GetLFTag' {tagKey} -> tagKey) (\s@GetLFTag' {} a -> s {tagKey = a} :: GetLFTag)

instance Core.AWSRequest GetLFTag where
  type AWSResponse GetLFTag = GetLFTagResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLFTagResponse'
            Prelude.<$> (x Core..?> "TagValues")
            Prelude.<*> (x Core..?> "CatalogId")
            Prelude.<*> (x Core..?> "TagKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLFTag

instance Prelude.NFData GetLFTag

instance Core.ToHeaders GetLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSLakeFormation.GetLFTag" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLFTag where
  toJSON GetLFTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Core..= tagKey)
          ]
      )

instance Core.ToPath GetLFTag where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLFTagResponse' smart constructor.
data GetLFTagResponse = GetLFTagResponse'
  { -- | A list of possible values an attribute can take.
    tagValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the tag.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLFTagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValues', 'getLFTagResponse_tagValues' - A list of possible values an attribute can take.
--
-- 'catalogId', 'getLFTagResponse_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'tagKey', 'getLFTagResponse_tagKey' - The key-name for the tag.
--
-- 'httpStatus', 'getLFTagResponse_httpStatus' - The response's http status code.
newGetLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLFTagResponse
newGetLFTagResponse pHttpStatus_ =
  GetLFTagResponse'
    { tagValues = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      tagKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of possible values an attribute can take.
getLFTagResponse_tagValues :: Lens.Lens' GetLFTagResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getLFTagResponse_tagValues = Lens.lens (\GetLFTagResponse' {tagValues} -> tagValues) (\s@GetLFTagResponse' {} a -> s {tagValues = a} :: GetLFTagResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
getLFTagResponse_catalogId :: Lens.Lens' GetLFTagResponse (Prelude.Maybe Prelude.Text)
getLFTagResponse_catalogId = Lens.lens (\GetLFTagResponse' {catalogId} -> catalogId) (\s@GetLFTagResponse' {} a -> s {catalogId = a} :: GetLFTagResponse)

-- | The key-name for the tag.
getLFTagResponse_tagKey :: Lens.Lens' GetLFTagResponse (Prelude.Maybe Prelude.Text)
getLFTagResponse_tagKey = Lens.lens (\GetLFTagResponse' {tagKey} -> tagKey) (\s@GetLFTagResponse' {} a -> s {tagKey = a} :: GetLFTagResponse)

-- | The response's http status code.
getLFTagResponse_httpStatus :: Lens.Lens' GetLFTagResponse Prelude.Int
getLFTagResponse_httpStatus = Lens.lens (\GetLFTagResponse' {httpStatus} -> httpStatus) (\s@GetLFTagResponse' {} a -> s {httpStatus = a} :: GetLFTagResponse)

instance Prelude.NFData GetLFTagResponse
