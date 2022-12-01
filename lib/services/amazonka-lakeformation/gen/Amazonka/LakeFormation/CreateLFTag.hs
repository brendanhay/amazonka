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
-- Module      : Amazonka.LakeFormation.CreateLFTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an LF-tag with the specified name and values.
module Amazonka.LakeFormation.CreateLFTag
  ( -- * Creating a Request
    CreateLFTag (..),
    newCreateLFTag,

    -- * Request Lenses
    createLFTag_catalogId,
    createLFTag_tagKey,
    createLFTag_tagValues,

    -- * Destructuring the Response
    CreateLFTagResponse (..),
    newCreateLFTagResponse,

    -- * Response Lenses
    createLFTagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLFTag' smart constructor.
data CreateLFTag = CreateLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the LF-tag.
    tagKey :: Prelude.Text,
    -- | A list of possible values an attribute can take.
    tagValues :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'createLFTag_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'tagKey', 'createLFTag_tagKey' - The key-name for the LF-tag.
--
-- 'tagValues', 'createLFTag_tagValues' - A list of possible values an attribute can take.
newCreateLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'tagValues'
  Prelude.NonEmpty Prelude.Text ->
  CreateLFTag
newCreateLFTag pTagKey_ pTagValues_ =
  CreateLFTag'
    { catalogId = Prelude.Nothing,
      tagKey = pTagKey_,
      tagValues = Lens.coerced Lens.# pTagValues_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
createLFTag_catalogId :: Lens.Lens' CreateLFTag (Prelude.Maybe Prelude.Text)
createLFTag_catalogId = Lens.lens (\CreateLFTag' {catalogId} -> catalogId) (\s@CreateLFTag' {} a -> s {catalogId = a} :: CreateLFTag)

-- | The key-name for the LF-tag.
createLFTag_tagKey :: Lens.Lens' CreateLFTag Prelude.Text
createLFTag_tagKey = Lens.lens (\CreateLFTag' {tagKey} -> tagKey) (\s@CreateLFTag' {} a -> s {tagKey = a} :: CreateLFTag)

-- | A list of possible values an attribute can take.
createLFTag_tagValues :: Lens.Lens' CreateLFTag (Prelude.NonEmpty Prelude.Text)
createLFTag_tagValues = Lens.lens (\CreateLFTag' {tagValues} -> tagValues) (\s@CreateLFTag' {} a -> s {tagValues = a} :: CreateLFTag) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLFTag where
  type AWSResponse CreateLFTag = CreateLFTagResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateLFTagResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLFTag where
  hashWithSalt _salt CreateLFTag' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData CreateLFTag where
  rnf CreateLFTag' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues

instance Core.ToHeaders CreateLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLFTag where
  toJSON CreateLFTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Core..= tagKey),
            Prelude.Just ("TagValues" Core..= tagValues)
          ]
      )

instance Core.ToPath CreateLFTag where
  toPath = Prelude.const "/CreateLFTag"

instance Core.ToQuery CreateLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLFTagResponse' smart constructor.
data CreateLFTagResponse = CreateLFTagResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLFTagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLFTagResponse_httpStatus' - The response's http status code.
newCreateLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLFTagResponse
newCreateLFTagResponse pHttpStatus_ =
  CreateLFTagResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createLFTagResponse_httpStatus :: Lens.Lens' CreateLFTagResponse Prelude.Int
createLFTagResponse_httpStatus = Lens.lens (\CreateLFTagResponse' {httpStatus} -> httpStatus) (\s@CreateLFTagResponse' {} a -> s {httpStatus = a} :: CreateLFTagResponse)

instance Prelude.NFData CreateLFTagResponse where
  rnf CreateLFTagResponse' {..} = Prelude.rnf httpStatus
