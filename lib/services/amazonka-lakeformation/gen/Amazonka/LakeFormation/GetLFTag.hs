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
-- Module      : Amazonka.LakeFormation.GetLFTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an LF-tag definition.
module Amazonka.LakeFormation.GetLFTag
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
    getLFTagResponse_catalogId,
    getLFTagResponse_tagKey,
    getLFTagResponse_tagValues,
    getLFTagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLFTag' smart constructor.
data GetLFTag = GetLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the LF-tag.
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
-- your Lake Formation environment.
--
-- 'tagKey', 'getLFTag_tagKey' - The key-name for the LF-tag.
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
-- your Lake Formation environment.
getLFTag_catalogId :: Lens.Lens' GetLFTag (Prelude.Maybe Prelude.Text)
getLFTag_catalogId = Lens.lens (\GetLFTag' {catalogId} -> catalogId) (\s@GetLFTag' {} a -> s {catalogId = a} :: GetLFTag)

-- | The key-name for the LF-tag.
getLFTag_tagKey :: Lens.Lens' GetLFTag Prelude.Text
getLFTag_tagKey = Lens.lens (\GetLFTag' {tagKey} -> tagKey) (\s@GetLFTag' {} a -> s {tagKey = a} :: GetLFTag)

instance Core.AWSRequest GetLFTag where
  type AWSResponse GetLFTag = GetLFTagResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLFTagResponse'
            Prelude.<$> (x Data..?> "CatalogId")
            Prelude.<*> (x Data..?> "TagKey")
            Prelude.<*> (x Data..?> "TagValues")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLFTag where
  hashWithSalt _salt GetLFTag' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tagKey

instance Prelude.NFData GetLFTag where
  rnf GetLFTag' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey

instance Data.ToHeaders GetLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLFTag where
  toJSON GetLFTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Data..= tagKey)
          ]
      )

instance Data.ToPath GetLFTag where
  toPath = Prelude.const "/GetLFTag"

instance Data.ToQuery GetLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLFTagResponse' smart constructor.
data GetLFTagResponse = GetLFTagResponse'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the LF-tag.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | A list of possible values an attribute can take.
    tagValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'catalogId', 'getLFTagResponse_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'tagKey', 'getLFTagResponse_tagKey' - The key-name for the LF-tag.
--
-- 'tagValues', 'getLFTagResponse_tagValues' - A list of possible values an attribute can take.
--
-- 'httpStatus', 'getLFTagResponse_httpStatus' - The response's http status code.
newGetLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLFTagResponse
newGetLFTagResponse pHttpStatus_ =
  GetLFTagResponse'
    { catalogId = Prelude.Nothing,
      tagKey = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
getLFTagResponse_catalogId :: Lens.Lens' GetLFTagResponse (Prelude.Maybe Prelude.Text)
getLFTagResponse_catalogId = Lens.lens (\GetLFTagResponse' {catalogId} -> catalogId) (\s@GetLFTagResponse' {} a -> s {catalogId = a} :: GetLFTagResponse)

-- | The key-name for the LF-tag.
getLFTagResponse_tagKey :: Lens.Lens' GetLFTagResponse (Prelude.Maybe Prelude.Text)
getLFTagResponse_tagKey = Lens.lens (\GetLFTagResponse' {tagKey} -> tagKey) (\s@GetLFTagResponse' {} a -> s {tagKey = a} :: GetLFTagResponse)

-- | A list of possible values an attribute can take.
getLFTagResponse_tagValues :: Lens.Lens' GetLFTagResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getLFTagResponse_tagValues = Lens.lens (\GetLFTagResponse' {tagValues} -> tagValues) (\s@GetLFTagResponse' {} a -> s {tagValues = a} :: GetLFTagResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLFTagResponse_httpStatus :: Lens.Lens' GetLFTagResponse Prelude.Int
getLFTagResponse_httpStatus = Lens.lens (\GetLFTagResponse' {httpStatus} -> httpStatus) (\s@GetLFTagResponse' {} a -> s {httpStatus = a} :: GetLFTagResponse)

instance Prelude.NFData GetLFTagResponse where
  rnf GetLFTagResponse' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf httpStatus
