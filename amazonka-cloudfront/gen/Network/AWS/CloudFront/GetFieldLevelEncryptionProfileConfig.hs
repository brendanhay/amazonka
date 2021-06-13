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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfileConfig
  ( -- * Creating a Request
    GetFieldLevelEncryptionProfileConfig (..),
    newGetFieldLevelEncryptionProfileConfig,

    -- * Request Lenses
    getFieldLevelEncryptionProfileConfig_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionProfileConfigResponse (..),
    newGetFieldLevelEncryptionProfileConfigResponse,

    -- * Response Lenses
    getFieldLevelEncryptionProfileConfigResponse_eTag,
    getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig,
    getFieldLevelEncryptionProfileConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileConfigResponse'
            Prelude.<$> (h Core..#? "ETag") Prelude.<*> (Core.parseXML x)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFieldLevelEncryptionProfileConfig

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileConfig

instance
  Core.ToHeaders
    GetFieldLevelEncryptionProfileConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetFieldLevelEncryptionProfileConfig
  where
  toPath GetFieldLevelEncryptionProfileConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Core.toBS id,
        "/config"
      ]

instance
  Core.ToQuery
    GetFieldLevelEncryptionProfileConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFieldLevelEncryptionProfileConfigResponse' smart constructor.
data GetFieldLevelEncryptionProfileConfigResponse = GetFieldLevelEncryptionProfileConfigResponse'
  { -- | The current version of the field-level encryption profile configuration
    -- result. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Return the field-level encryption profile configuration information.
    fieldLevelEncryptionProfileConfig :: Prelude.Maybe FieldLevelEncryptionProfileConfig,
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
-- 'eTag', 'getFieldLevelEncryptionProfileConfigResponse_eTag' - The current version of the field-level encryption profile configuration
-- result. For example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionProfileConfig', 'getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig' - Return the field-level encryption profile configuration information.
--
-- 'httpStatus', 'getFieldLevelEncryptionProfileConfigResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionProfileConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFieldLevelEncryptionProfileConfigResponse
newGetFieldLevelEncryptionProfileConfigResponse
  pHttpStatus_ =
    GetFieldLevelEncryptionProfileConfigResponse'
      { eTag =
          Prelude.Nothing,
        fieldLevelEncryptionProfileConfig =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field-level encryption profile configuration
-- result. For example: @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionProfileConfigResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Prelude.Maybe Prelude.Text)
getFieldLevelEncryptionProfileConfigResponse_eTag = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionProfileConfigResponse)

-- | Return the field-level encryption profile configuration information.
getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse (Prelude.Maybe FieldLevelEncryptionProfileConfig)
getFieldLevelEncryptionProfileConfigResponse_fieldLevelEncryptionProfileConfig = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {fieldLevelEncryptionProfileConfig} -> fieldLevelEncryptionProfileConfig) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {fieldLevelEncryptionProfileConfig = a} :: GetFieldLevelEncryptionProfileConfigResponse)

-- | The response's http status code.
getFieldLevelEncryptionProfileConfigResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionProfileConfigResponse Prelude.Int
getFieldLevelEncryptionProfileConfigResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionProfileConfigResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionProfileConfigResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionProfileConfigResponse)

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileConfigResponse
