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
-- Module      : Amazonka.CloudFront.GetFieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile configuration information.
module Amazonka.CloudFront.GetFieldLevelEncryptionProfileConfig
  ( -- * Creating a Request
    GetFieldLevelEncryptionProfileConfig (..),
    newGetFieldLevelEncryptionProfileConfig,

    -- * Request Lenses
    getFieldLevelEncryptionProfileConfig_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionProfileConfigResponse (..),
    newGetFieldLevelEncryptionProfileConfigResponse,

    -- * Response Lenses
    getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig,
    getFieldLevelEncryptionProfileConfigResponse_eTag,
    getFieldLevelEncryptionProfileConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFieldLevelEncryptionProfileConfig' smart constructor.
data GetFieldLevelEncryptionProfileConfig = GetFieldLevelEncryptionProfileConfig'
  { -- | Get the ID for the field-level encryption profile configuration
    -- information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionProfileConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFieldLevelEncryptionProfileConfig_id' - Get the ID for the field-level encryption profile configuration
-- information.
newGetFieldLevelEncryptionProfileConfig ::
  -- | 'id'
  Prelude.Text ->
  GetFieldLevelEncryptionProfileConfig
newGetFieldLevelEncryptionProfileConfig pId_ =
  GetFieldLevelEncryptionProfileConfig' {id = pId_}

-- | Get the ID for the field-level encryption profile configuration
-- information.
getFieldLevelEncryptionProfileConfig_id :: Lens.Lens' GetFieldLevelEncryptionProfileConfig Prelude.Text
getFieldLevelEncryptionProfileConfig_id = Lens.lens (\GetFieldLevelEncryptionProfileConfig' {id} -> id) (\s@GetFieldLevelEncryptionProfileConfig' {} a -> s {id = a} :: GetFieldLevelEncryptionProfileConfig)

instance
  Core.AWSRequest
    GetFieldLevelEncryptionProfileConfig
  where
  type
    AWSResponse GetFieldLevelEncryptionProfileConfig =
      GetFieldLevelEncryptionProfileConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileConfigResponse'
            Prelude.<$> (Data.parseXML x) Prelude.<*> (h Data..#? "ETag")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFieldLevelEncryptionProfileConfig
  where
  hashWithSalt
    _salt
    GetFieldLevelEncryptionProfileConfig' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileConfig
  where
  rnf GetFieldLevelEncryptionProfileConfig' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetFieldLevelEncryptionProfileConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetFieldLevelEncryptionProfileConfig
  where
  toPath GetFieldLevelEncryptionProfileConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Data.toBS id,
        "/config"
      ]

instance
  Data.ToQuery
    GetFieldLevelEncryptionProfileConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFieldLevelEncryptionProfileConfigResponse' smart constructor.
data GetFieldLevelEncryptionProfileConfigResponse = GetFieldLevelEncryptionProfileConfigResponse'
  { -- | Return the field-level encryption profile configuration information.
    fieldLevelEncryptionProfileConfig :: Prelude.Maybe FieldLevelEncryptionProfileConfig,
    -- | The current version of the field-level encryption profile configuration
    -- result. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionProfileConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionProfileConfig', 'getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig' - Return the field-level encryption profile configuration information.
--
-- 'eTag', 'getFieldLevelEncryptionProfileConfigResponse_eTag' - The current version of the field-level encryption profile configuration
-- result. For example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'getFieldLevelEncryptionProfileConfigResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionProfileConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFieldLevelEncryptionProfileConfigResponse
newGetFieldLevelEncryptionProfileConfigResponse
  pHttpStatus_ =
    GetFieldLevelEncryptionProfileConfigResponse'
      { fieldLevelEncryptionProfileConfig =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Return the field-level encryption profile configuration information.
getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Prelude.Maybe FieldLevelEncryptionProfileConfig)
getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {fieldLevelEncryptionProfileConfig} -> fieldLevelEncryptionProfileConfig) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {fieldLevelEncryptionProfileConfig = a} :: GetFieldLevelEncryptionProfileConfigResponse)

-- | The current version of the field-level encryption profile configuration
-- result. For example: @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionProfileConfigResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Prelude.Maybe Prelude.Text)
getFieldLevelEncryptionProfileConfigResponse_eTag = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionProfileConfigResponse)

-- | The response's http status code.
getFieldLevelEncryptionProfileConfigResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse Prelude.Int
getFieldLevelEncryptionProfileConfigResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionProfileConfigResponse)

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileConfigResponse
  where
  rnf GetFieldLevelEncryptionProfileConfigResponse' {..} =
    Prelude.rnf fieldLevelEncryptionProfileConfig
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
