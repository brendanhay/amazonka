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
-- Module      : Amazonka.LakeFormation.DeleteLFTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified LF-tag given a key name. If the input parameter
-- tag key was not found, then the operation will throw an exception. When
-- you delete an LF-tag, the @LFTagPolicy@ attached to the LF-tag becomes
-- invalid. If the deleted LF-tag was still assigned to any resource, the
-- tag policy attach to the deleted LF-tag will no longer be applied to the
-- resource.
module Amazonka.LakeFormation.DeleteLFTag
  ( -- * Creating a Request
    DeleteLFTag (..),
    newDeleteLFTag,

    -- * Request Lenses
    deleteLFTag_catalogId,
    deleteLFTag_tagKey,

    -- * Destructuring the Response
    DeleteLFTagResponse (..),
    newDeleteLFTagResponse,

    -- * Response Lenses
    deleteLFTagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLFTag' smart constructor.
data DeleteLFTag = DeleteLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The key-name for the LF-tag to delete.
    tagKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteLFTag_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
--
-- 'tagKey', 'deleteLFTag_tagKey' - The key-name for the LF-tag to delete.
newDeleteLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  DeleteLFTag
newDeleteLFTag pTagKey_ =
  DeleteLFTag'
    { catalogId = Prelude.Nothing,
      tagKey = pTagKey_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your Lake Formation environment.
deleteLFTag_catalogId :: Lens.Lens' DeleteLFTag (Prelude.Maybe Prelude.Text)
deleteLFTag_catalogId = Lens.lens (\DeleteLFTag' {catalogId} -> catalogId) (\s@DeleteLFTag' {} a -> s {catalogId = a} :: DeleteLFTag)

-- | The key-name for the LF-tag to delete.
deleteLFTag_tagKey :: Lens.Lens' DeleteLFTag Prelude.Text
deleteLFTag_tagKey = Lens.lens (\DeleteLFTag' {tagKey} -> tagKey) (\s@DeleteLFTag' {} a -> s {tagKey = a} :: DeleteLFTag)

instance Core.AWSRequest DeleteLFTag where
  type AWSResponse DeleteLFTag = DeleteLFTagResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLFTagResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLFTag where
  hashWithSalt _salt DeleteLFTag' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tagKey

instance Prelude.NFData DeleteLFTag where
  rnf DeleteLFTag' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey

instance Data.ToHeaders DeleteLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLFTag where
  toJSON DeleteLFTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("TagKey" Data..= tagKey)
          ]
      )

instance Data.ToPath DeleteLFTag where
  toPath = Prelude.const "/DeleteLFTag"

instance Data.ToQuery DeleteLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLFTagResponse' smart constructor.
data DeleteLFTagResponse = DeleteLFTagResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLFTagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLFTagResponse_httpStatus' - The response's http status code.
newDeleteLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLFTagResponse
newDeleteLFTagResponse pHttpStatus_ =
  DeleteLFTagResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLFTagResponse_httpStatus :: Lens.Lens' DeleteLFTagResponse Prelude.Int
deleteLFTagResponse_httpStatus = Lens.lens (\DeleteLFTagResponse' {httpStatus} -> httpStatus) (\s@DeleteLFTagResponse' {} a -> s {httpStatus = a} :: DeleteLFTagResponse)

instance Prelude.NFData DeleteLFTagResponse where
  rnf DeleteLFTagResponse' {..} = Prelude.rnf httpStatus
