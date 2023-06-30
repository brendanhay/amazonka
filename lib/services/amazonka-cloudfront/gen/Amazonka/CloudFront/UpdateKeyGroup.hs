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
-- Module      : Amazonka.CloudFront.UpdateKeyGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a key group.
--
-- When you update a key group, all the fields are updated with the values
-- provided in the request. You cannot update some fields independent of
-- others. To update a key group:
--
-- 1.  Get the current key group with @GetKeyGroup@ or @GetKeyGroupConfig@.
--
-- 2.  Locally modify the fields in the key group that you want to update.
--     For example, add or remove public key IDs.
--
-- 3.  Call @UpdateKeyGroup@ with the entire key group object, including
--     the fields that you modified and those that you didn\'t.
module Amazonka.CloudFront.UpdateKeyGroup
  ( -- * Creating a Request
    UpdateKeyGroup (..),
    newUpdateKeyGroup,

    -- * Request Lenses
    updateKeyGroup_ifMatch,
    updateKeyGroup_keyGroupConfig,
    updateKeyGroup_id,

    -- * Destructuring the Response
    UpdateKeyGroupResponse (..),
    newUpdateKeyGroupResponse,

    -- * Response Lenses
    updateKeyGroupResponse_eTag,
    updateKeyGroupResponse_keyGroup,
    updateKeyGroupResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKeyGroup' smart constructor.
data UpdateKeyGroup = UpdateKeyGroup'
  { -- | The version of the key group that you are updating. The version is the
    -- key group\'s @ETag@ value.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The key group configuration.
    keyGroupConfig :: KeyGroupConfig,
    -- | The identifier of the key group that you are updating.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKeyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateKeyGroup_ifMatch' - The version of the key group that you are updating. The version is the
-- key group\'s @ETag@ value.
--
-- 'keyGroupConfig', 'updateKeyGroup_keyGroupConfig' - The key group configuration.
--
-- 'id', 'updateKeyGroup_id' - The identifier of the key group that you are updating.
newUpdateKeyGroup ::
  -- | 'keyGroupConfig'
  KeyGroupConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateKeyGroup
newUpdateKeyGroup pKeyGroupConfig_ pId_ =
  UpdateKeyGroup'
    { ifMatch = Prelude.Nothing,
      keyGroupConfig = pKeyGroupConfig_,
      id = pId_
    }

-- | The version of the key group that you are updating. The version is the
-- key group\'s @ETag@ value.
updateKeyGroup_ifMatch :: Lens.Lens' UpdateKeyGroup (Prelude.Maybe Prelude.Text)
updateKeyGroup_ifMatch = Lens.lens (\UpdateKeyGroup' {ifMatch} -> ifMatch) (\s@UpdateKeyGroup' {} a -> s {ifMatch = a} :: UpdateKeyGroup)

-- | The key group configuration.
updateKeyGroup_keyGroupConfig :: Lens.Lens' UpdateKeyGroup KeyGroupConfig
updateKeyGroup_keyGroupConfig = Lens.lens (\UpdateKeyGroup' {keyGroupConfig} -> keyGroupConfig) (\s@UpdateKeyGroup' {} a -> s {keyGroupConfig = a} :: UpdateKeyGroup)

-- | The identifier of the key group that you are updating.
updateKeyGroup_id :: Lens.Lens' UpdateKeyGroup Prelude.Text
updateKeyGroup_id = Lens.lens (\UpdateKeyGroup' {id} -> id) (\s@UpdateKeyGroup' {} a -> s {id = a} :: UpdateKeyGroup)

instance Core.AWSRequest UpdateKeyGroup where
  type
    AWSResponse UpdateKeyGroup =
      UpdateKeyGroupResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateKeyGroupResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateKeyGroup where
  hashWithSalt _salt UpdateKeyGroup' {..} =
    _salt
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` keyGroupConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateKeyGroup where
  rnf UpdateKeyGroup' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf keyGroupConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateKeyGroup where
  toElement UpdateKeyGroup' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}KeyGroupConfig"
      keyGroupConfig

instance Data.ToHeaders UpdateKeyGroup where
  toHeaders UpdateKeyGroup' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateKeyGroup where
  toPath UpdateKeyGroup' {..} =
    Prelude.mconcat
      ["/2020-05-31/key-group/", Data.toBS id]

instance Data.ToQuery UpdateKeyGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKeyGroupResponse' smart constructor.
data UpdateKeyGroupResponse = UpdateKeyGroupResponse'
  { -- | The identifier for this version of the key group.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The key group that was just updated.
    keyGroup :: Prelude.Maybe KeyGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKeyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateKeyGroupResponse_eTag' - The identifier for this version of the key group.
--
-- 'keyGroup', 'updateKeyGroupResponse_keyGroup' - The key group that was just updated.
--
-- 'httpStatus', 'updateKeyGroupResponse_httpStatus' - The response's http status code.
newUpdateKeyGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateKeyGroupResponse
newUpdateKeyGroupResponse pHttpStatus_ =
  UpdateKeyGroupResponse'
    { eTag = Prelude.Nothing,
      keyGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier for this version of the key group.
updateKeyGroupResponse_eTag :: Lens.Lens' UpdateKeyGroupResponse (Prelude.Maybe Prelude.Text)
updateKeyGroupResponse_eTag = Lens.lens (\UpdateKeyGroupResponse' {eTag} -> eTag) (\s@UpdateKeyGroupResponse' {} a -> s {eTag = a} :: UpdateKeyGroupResponse)

-- | The key group that was just updated.
updateKeyGroupResponse_keyGroup :: Lens.Lens' UpdateKeyGroupResponse (Prelude.Maybe KeyGroup)
updateKeyGroupResponse_keyGroup = Lens.lens (\UpdateKeyGroupResponse' {keyGroup} -> keyGroup) (\s@UpdateKeyGroupResponse' {} a -> s {keyGroup = a} :: UpdateKeyGroupResponse)

-- | The response's http status code.
updateKeyGroupResponse_httpStatus :: Lens.Lens' UpdateKeyGroupResponse Prelude.Int
updateKeyGroupResponse_httpStatus = Lens.lens (\UpdateKeyGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateKeyGroupResponse' {} a -> s {httpStatus = a} :: UpdateKeyGroupResponse)

instance Prelude.NFData UpdateKeyGroupResponse where
  rnf UpdateKeyGroupResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf keyGroup
      `Prelude.seq` Prelude.rnf httpStatus
