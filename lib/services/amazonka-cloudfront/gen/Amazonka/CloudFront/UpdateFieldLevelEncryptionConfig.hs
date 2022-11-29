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
-- Module      : Amazonka.CloudFront.UpdateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption configuration.
module Amazonka.CloudFront.UpdateFieldLevelEncryptionConfig
  ( -- * Creating a Request
    UpdateFieldLevelEncryptionConfig (..),
    newUpdateFieldLevelEncryptionConfig,

    -- * Request Lenses
    updateFieldLevelEncryptionConfig_ifMatch,
    updateFieldLevelEncryptionConfig_fieldLevelEncryptionConfig,
    updateFieldLevelEncryptionConfig_id,

    -- * Destructuring the Response
    UpdateFieldLevelEncryptionConfigResponse (..),
    newUpdateFieldLevelEncryptionConfigResponse,

    -- * Response Lenses
    updateFieldLevelEncryptionConfigResponse_eTag,
    updateFieldLevelEncryptionConfigResponse_fieldLevelEncryption,
    updateFieldLevelEncryptionConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFieldLevelEncryptionConfig' smart constructor.
data UpdateFieldLevelEncryptionConfig = UpdateFieldLevelEncryptionConfig'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- configuration identity to update. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | Request to update a field-level encryption configuration.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig,
    -- | The ID of the configuration you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateFieldLevelEncryptionConfig_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- configuration identity to update. For example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionConfig', 'updateFieldLevelEncryptionConfig_fieldLevelEncryptionConfig' - Request to update a field-level encryption configuration.
--
-- 'id', 'updateFieldLevelEncryptionConfig_id' - The ID of the configuration you want to update.
newUpdateFieldLevelEncryptionConfig ::
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateFieldLevelEncryptionConfig
newUpdateFieldLevelEncryptionConfig
  pFieldLevelEncryptionConfig_
  pId_ =
    UpdateFieldLevelEncryptionConfig'
      { ifMatch =
          Prelude.Nothing,
        fieldLevelEncryptionConfig =
          pFieldLevelEncryptionConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- configuration identity to update. For example: @E2QWRUHAPOMQZL@.
updateFieldLevelEncryptionConfig_ifMatch :: Lens.Lens' UpdateFieldLevelEncryptionConfig (Prelude.Maybe Prelude.Text)
updateFieldLevelEncryptionConfig_ifMatch = Lens.lens (\UpdateFieldLevelEncryptionConfig' {ifMatch} -> ifMatch) (\s@UpdateFieldLevelEncryptionConfig' {} a -> s {ifMatch = a} :: UpdateFieldLevelEncryptionConfig)

-- | Request to update a field-level encryption configuration.
updateFieldLevelEncryptionConfig_fieldLevelEncryptionConfig :: Lens.Lens' UpdateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
updateFieldLevelEncryptionConfig_fieldLevelEncryptionConfig = Lens.lens (\UpdateFieldLevelEncryptionConfig' {fieldLevelEncryptionConfig} -> fieldLevelEncryptionConfig) (\s@UpdateFieldLevelEncryptionConfig' {} a -> s {fieldLevelEncryptionConfig = a} :: UpdateFieldLevelEncryptionConfig)

-- | The ID of the configuration you want to update.
updateFieldLevelEncryptionConfig_id :: Lens.Lens' UpdateFieldLevelEncryptionConfig Prelude.Text
updateFieldLevelEncryptionConfig_id = Lens.lens (\UpdateFieldLevelEncryptionConfig' {id} -> id) (\s@UpdateFieldLevelEncryptionConfig' {} a -> s {id = a} :: UpdateFieldLevelEncryptionConfig)

instance
  Core.AWSRequest
    UpdateFieldLevelEncryptionConfig
  where
  type
    AWSResponse UpdateFieldLevelEncryptionConfig =
      UpdateFieldLevelEncryptionConfigResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionConfigResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFieldLevelEncryptionConfig
  where
  hashWithSalt
    _salt
    UpdateFieldLevelEncryptionConfig' {..} =
      _salt `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` fieldLevelEncryptionConfig
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionConfig
  where
  rnf UpdateFieldLevelEncryptionConfig' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf fieldLevelEncryptionConfig
      `Prelude.seq` Prelude.rnf id

instance
  Core.ToElement
    UpdateFieldLevelEncryptionConfig
  where
  toElement UpdateFieldLevelEncryptionConfig' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionConfig"
      fieldLevelEncryptionConfig

instance
  Core.ToHeaders
    UpdateFieldLevelEncryptionConfig
  where
  toHeaders UpdateFieldLevelEncryptionConfig' {..} =
    Prelude.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdateFieldLevelEncryptionConfig where
  toPath UpdateFieldLevelEncryptionConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption/",
        Core.toBS id,
        "/config"
      ]

instance
  Core.ToQuery
    UpdateFieldLevelEncryptionConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFieldLevelEncryptionConfigResponse' smart constructor.
data UpdateFieldLevelEncryptionConfigResponse = UpdateFieldLevelEncryptionConfigResponse'
  { -- | The value of the @ETag@ header that you received when updating the
    -- configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Return the results of updating the configuration.
    fieldLevelEncryption :: Prelude.Maybe FieldLevelEncryption,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldLevelEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateFieldLevelEncryptionConfigResponse_eTag' - The value of the @ETag@ header that you received when updating the
-- configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryption', 'updateFieldLevelEncryptionConfigResponse_fieldLevelEncryption' - Return the results of updating the configuration.
--
-- 'httpStatus', 'updateFieldLevelEncryptionConfigResponse_httpStatus' - The response's http status code.
newUpdateFieldLevelEncryptionConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFieldLevelEncryptionConfigResponse
newUpdateFieldLevelEncryptionConfigResponse
  pHttpStatus_ =
    UpdateFieldLevelEncryptionConfigResponse'
      { eTag =
          Prelude.Nothing,
        fieldLevelEncryption =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The value of the @ETag@ header that you received when updating the
-- configuration. For example: @E2QWRUHAPOMQZL@.
updateFieldLevelEncryptionConfigResponse_eTag :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Prelude.Maybe Prelude.Text)
updateFieldLevelEncryptionConfigResponse_eTag = Lens.lens (\UpdateFieldLevelEncryptionConfigResponse' {eTag} -> eTag) (\s@UpdateFieldLevelEncryptionConfigResponse' {} a -> s {eTag = a} :: UpdateFieldLevelEncryptionConfigResponse)

-- | Return the results of updating the configuration.
updateFieldLevelEncryptionConfigResponse_fieldLevelEncryption :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse (Prelude.Maybe FieldLevelEncryption)
updateFieldLevelEncryptionConfigResponse_fieldLevelEncryption = Lens.lens (\UpdateFieldLevelEncryptionConfigResponse' {fieldLevelEncryption} -> fieldLevelEncryption) (\s@UpdateFieldLevelEncryptionConfigResponse' {} a -> s {fieldLevelEncryption = a} :: UpdateFieldLevelEncryptionConfigResponse)

-- | The response's http status code.
updateFieldLevelEncryptionConfigResponse_httpStatus :: Lens.Lens' UpdateFieldLevelEncryptionConfigResponse Prelude.Int
updateFieldLevelEncryptionConfigResponse_httpStatus = Lens.lens (\UpdateFieldLevelEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateFieldLevelEncryptionConfigResponse' {} a -> s {httpStatus = a} :: UpdateFieldLevelEncryptionConfigResponse)

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionConfigResponse
  where
  rnf UpdateFieldLevelEncryptionConfigResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf fieldLevelEncryption
      `Prelude.seq` Prelude.rnf httpStatus
