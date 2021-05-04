{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionConfig
  ( -- * Creating a Request
    GetFieldLevelEncryptionConfig (..),
    newGetFieldLevelEncryptionConfig,

    -- * Request Lenses
    getFieldLevelEncryptionConfig_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionConfigResponse (..),
    newGetFieldLevelEncryptionConfigResponse,

    -- * Response Lenses
    getFieldLevelEncryptionConfigResponse_eTag,
    getFieldLevelEncryptionConfigResponse_fieldLevelEncryptionConfig,
    getFieldLevelEncryptionConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFieldLevelEncryptionConfig' smart constructor.
data GetFieldLevelEncryptionConfig = GetFieldLevelEncryptionConfig'
  { -- | Request the ID for the field-level encryption configuration information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFieldLevelEncryptionConfig_id' - Request the ID for the field-level encryption configuration information.
newGetFieldLevelEncryptionConfig ::
  -- | 'id'
  Prelude.Text ->
  GetFieldLevelEncryptionConfig
newGetFieldLevelEncryptionConfig pId_ =
  GetFieldLevelEncryptionConfig' {id = pId_}

-- | Request the ID for the field-level encryption configuration information.
getFieldLevelEncryptionConfig_id :: Lens.Lens' GetFieldLevelEncryptionConfig Prelude.Text
getFieldLevelEncryptionConfig_id = Lens.lens (\GetFieldLevelEncryptionConfig' {id} -> id) (\s@GetFieldLevelEncryptionConfig' {} a -> s {id = a} :: GetFieldLevelEncryptionConfig)

instance
  Prelude.AWSRequest
    GetFieldLevelEncryptionConfig
  where
  type
    Rs GetFieldLevelEncryptionConfig =
      GetFieldLevelEncryptionConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionConfigResponse'
            Prelude.<$> (h Prelude..#? "ETag")
            Prelude.<*> (Prelude.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFieldLevelEncryptionConfig

instance Prelude.NFData GetFieldLevelEncryptionConfig

instance
  Prelude.ToHeaders
    GetFieldLevelEncryptionConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetFieldLevelEncryptionConfig where
  toPath GetFieldLevelEncryptionConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption/",
        Prelude.toBS id,
        "/config"
      ]

instance
  Prelude.ToQuery
    GetFieldLevelEncryptionConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFieldLevelEncryptionConfigResponse' smart constructor.
data GetFieldLevelEncryptionConfigResponse = GetFieldLevelEncryptionConfigResponse'
  { -- | The current version of the field level encryption configuration. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Return the field-level encryption configuration information.
    fieldLevelEncryptionConfig :: Prelude.Maybe FieldLevelEncryptionConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getFieldLevelEncryptionConfigResponse_eTag' - The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionConfig', 'getFieldLevelEncryptionConfigResponse_fieldLevelEncryptionConfig' - Return the field-level encryption configuration information.
--
-- 'httpStatus', 'getFieldLevelEncryptionConfigResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFieldLevelEncryptionConfigResponse
newGetFieldLevelEncryptionConfigResponse pHttpStatus_ =
  GetFieldLevelEncryptionConfigResponse'
    { eTag =
        Prelude.Nothing,
      fieldLevelEncryptionConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionConfigResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Prelude.Maybe Prelude.Text)
getFieldLevelEncryptionConfigResponse_eTag = Lens.lens (\GetFieldLevelEncryptionConfigResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionConfigResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionConfigResponse)

-- | Return the field-level encryption configuration information.
getFieldLevelEncryptionConfigResponse_fieldLevelEncryptionConfig :: Lens.Lens' GetFieldLevelEncryptionConfigResponse (Prelude.Maybe FieldLevelEncryptionConfig)
getFieldLevelEncryptionConfigResponse_fieldLevelEncryptionConfig = Lens.lens (\GetFieldLevelEncryptionConfigResponse' {fieldLevelEncryptionConfig} -> fieldLevelEncryptionConfig) (\s@GetFieldLevelEncryptionConfigResponse' {} a -> s {fieldLevelEncryptionConfig = a} :: GetFieldLevelEncryptionConfigResponse)

-- | The response's http status code.
getFieldLevelEncryptionConfigResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionConfigResponse Prelude.Int
getFieldLevelEncryptionConfigResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionConfigResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionConfigResponse)

instance
  Prelude.NFData
    GetFieldLevelEncryptionConfigResponse
