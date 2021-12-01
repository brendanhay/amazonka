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
-- Module      : Amazonka.LakeFormation.UpdateLFTag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the list of possible values for the specified tag key. If the
-- tag does not exist, the operation throws an EntityNotFoundException. The
-- values in the delete key values will be deleted from list of possible
-- values. If any value in the delete key values is attached to a resource,
-- then API errors out with a 400 Exception - \"Update not allowed\". Untag
-- the attribute before deleting the tag key\'s value.
module Amazonka.LakeFormation.UpdateLFTag
  ( -- * Creating a Request
    UpdateLFTag (..),
    newUpdateLFTag,

    -- * Request Lenses
    updateLFTag_catalogId,
    updateLFTag_tagValuesToAdd,
    updateLFTag_tagValuesToDelete,
    updateLFTag_tagKey,

    -- * Destructuring the Response
    UpdateLFTagResponse (..),
    newUpdateLFTagResponse,

    -- * Response Lenses
    updateLFTagResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLFTag' smart constructor.
data UpdateLFTag = UpdateLFTag'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A list of tag values to add from the tag.
    tagValuesToAdd :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of tag values to delete from the tag.
    tagValuesToDelete :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The key-name for the tag for which to add or delete values.
    tagKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLFTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateLFTag_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'tagValuesToAdd', 'updateLFTag_tagValuesToAdd' - A list of tag values to add from the tag.
--
-- 'tagValuesToDelete', 'updateLFTag_tagValuesToDelete' - A list of tag values to delete from the tag.
--
-- 'tagKey', 'updateLFTag_tagKey' - The key-name for the tag for which to add or delete values.
newUpdateLFTag ::
  -- | 'tagKey'
  Prelude.Text ->
  UpdateLFTag
newUpdateLFTag pTagKey_ =
  UpdateLFTag'
    { catalogId = Prelude.Nothing,
      tagValuesToAdd = Prelude.Nothing,
      tagValuesToDelete = Prelude.Nothing,
      tagKey = pTagKey_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
updateLFTag_catalogId :: Lens.Lens' UpdateLFTag (Prelude.Maybe Prelude.Text)
updateLFTag_catalogId = Lens.lens (\UpdateLFTag' {catalogId} -> catalogId) (\s@UpdateLFTag' {} a -> s {catalogId = a} :: UpdateLFTag)

-- | A list of tag values to add from the tag.
updateLFTag_tagValuesToAdd :: Lens.Lens' UpdateLFTag (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLFTag_tagValuesToAdd = Lens.lens (\UpdateLFTag' {tagValuesToAdd} -> tagValuesToAdd) (\s@UpdateLFTag' {} a -> s {tagValuesToAdd = a} :: UpdateLFTag) Prelude.. Lens.mapping Lens.coerced

-- | A list of tag values to delete from the tag.
updateLFTag_tagValuesToDelete :: Lens.Lens' UpdateLFTag (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLFTag_tagValuesToDelete = Lens.lens (\UpdateLFTag' {tagValuesToDelete} -> tagValuesToDelete) (\s@UpdateLFTag' {} a -> s {tagValuesToDelete = a} :: UpdateLFTag) Prelude.. Lens.mapping Lens.coerced

-- | The key-name for the tag for which to add or delete values.
updateLFTag_tagKey :: Lens.Lens' UpdateLFTag Prelude.Text
updateLFTag_tagKey = Lens.lens (\UpdateLFTag' {tagKey} -> tagKey) (\s@UpdateLFTag' {} a -> s {tagKey = a} :: UpdateLFTag)

instance Core.AWSRequest UpdateLFTag where
  type AWSResponse UpdateLFTag = UpdateLFTagResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLFTagResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLFTag where
  hashWithSalt salt' UpdateLFTag' {..} =
    salt' `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValuesToDelete
      `Prelude.hashWithSalt` tagValuesToAdd
      `Prelude.hashWithSalt` catalogId

instance Prelude.NFData UpdateLFTag where
  rnf UpdateLFTag' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValuesToDelete
      `Prelude.seq` Prelude.rnf tagValuesToAdd

instance Core.ToHeaders UpdateLFTag where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.UpdateLFTag" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateLFTag where
  toJSON UpdateLFTag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("TagValuesToAdd" Core..=)
              Prelude.<$> tagValuesToAdd,
            ("TagValuesToDelete" Core..=)
              Prelude.<$> tagValuesToDelete,
            Prelude.Just ("TagKey" Core..= tagKey)
          ]
      )

instance Core.ToPath UpdateLFTag where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateLFTag where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLFTagResponse' smart constructor.
data UpdateLFTagResponse = UpdateLFTagResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLFTagResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLFTagResponse_httpStatus' - The response's http status code.
newUpdateLFTagResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLFTagResponse
newUpdateLFTagResponse pHttpStatus_ =
  UpdateLFTagResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateLFTagResponse_httpStatus :: Lens.Lens' UpdateLFTagResponse Prelude.Int
updateLFTagResponse_httpStatus = Lens.lens (\UpdateLFTagResponse' {httpStatus} -> httpStatus) (\s@UpdateLFTagResponse' {} a -> s {httpStatus = a} :: UpdateLFTagResponse)

instance Prelude.NFData UpdateLFTagResponse where
  rnf UpdateLFTagResponse' {..} = Prelude.rnf httpStatus
