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
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new field-level encryption configuration.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionConfig
  ( -- * Creating a Request
    CreateFieldLevelEncryptionConfig (..),
    newCreateFieldLevelEncryptionConfig,

    -- * Request Lenses
    createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig,

    -- * Destructuring the Response
    CreateFieldLevelEncryptionConfigResponse (..),
    newCreateFieldLevelEncryptionConfigResponse,

    -- * Response Lenses
    createFieldLevelEncryptionConfigResponse_eTag,
    createFieldLevelEncryptionConfigResponse_fieldLevelEncryption,
    createFieldLevelEncryptionConfigResponse_location,
    createFieldLevelEncryptionConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFieldLevelEncryptionConfig' smart constructor.
data CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { -- | The request to create a new field-level encryption configuration.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFieldLevelEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionConfig', 'createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig' - The request to create a new field-level encryption configuration.
newCreateFieldLevelEncryptionConfig ::
  -- | 'fieldLevelEncryptionConfig'
  FieldLevelEncryptionConfig ->
  CreateFieldLevelEncryptionConfig
newCreateFieldLevelEncryptionConfig
  pFieldLevelEncryptionConfig_ =
    CreateFieldLevelEncryptionConfig'
      { fieldLevelEncryptionConfig =
          pFieldLevelEncryptionConfig_
      }

-- | The request to create a new field-level encryption configuration.
createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig :: Lens.Lens' CreateFieldLevelEncryptionConfig FieldLevelEncryptionConfig
createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig = Lens.lens (\CreateFieldLevelEncryptionConfig' {fieldLevelEncryptionConfig} -> fieldLevelEncryptionConfig) (\s@CreateFieldLevelEncryptionConfig' {} a -> s {fieldLevelEncryptionConfig = a} :: CreateFieldLevelEncryptionConfig)

instance
  Core.AWSRequest
    CreateFieldLevelEncryptionConfig
  where
  type
    AWSResponse CreateFieldLevelEncryptionConfig =
      CreateFieldLevelEncryptionConfigResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionConfigResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateFieldLevelEncryptionConfig

instance Core.NFData CreateFieldLevelEncryptionConfig

instance
  Core.ToElement
    CreateFieldLevelEncryptionConfig
  where
  toElement CreateFieldLevelEncryptionConfig' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionConfig"
      fieldLevelEncryptionConfig

instance
  Core.ToHeaders
    CreateFieldLevelEncryptionConfig
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateFieldLevelEncryptionConfig where
  toPath =
    Core.const "/2020-05-31/field-level-encryption"

instance
  Core.ToQuery
    CreateFieldLevelEncryptionConfig
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { -- | The current version of the field level encryption configuration. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | Returned when you create a new field-level encryption configuration.
    fieldLevelEncryption :: Core.Maybe FieldLevelEncryption,
    -- | The fully qualified URI of the new configuration resource just created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFieldLevelEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createFieldLevelEncryptionConfigResponse_eTag' - The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryption', 'createFieldLevelEncryptionConfigResponse_fieldLevelEncryption' - Returned when you create a new field-level encryption configuration.
--
-- 'location', 'createFieldLevelEncryptionConfigResponse_location' - The fully qualified URI of the new configuration resource just created.
--
-- 'httpStatus', 'createFieldLevelEncryptionConfigResponse_httpStatus' - The response's http status code.
newCreateFieldLevelEncryptionConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateFieldLevelEncryptionConfigResponse
newCreateFieldLevelEncryptionConfigResponse
  pHttpStatus_ =
    CreateFieldLevelEncryptionConfigResponse'
      { eTag =
          Core.Nothing,
        fieldLevelEncryption =
          Core.Nothing,
        location = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
createFieldLevelEncryptionConfigResponse_eTag :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
createFieldLevelEncryptionConfigResponse_eTag = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {eTag} -> eTag) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {eTag = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | Returned when you create a new field-level encryption configuration.
createFieldLevelEncryptionConfigResponse_fieldLevelEncryption :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe FieldLevelEncryption)
createFieldLevelEncryptionConfigResponse_fieldLevelEncryption = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {fieldLevelEncryption} -> fieldLevelEncryption) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {fieldLevelEncryption = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | The fully qualified URI of the new configuration resource just created.
createFieldLevelEncryptionConfigResponse_location :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Core.Maybe Core.Text)
createFieldLevelEncryptionConfigResponse_location = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {location} -> location) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {location = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | The response's http status code.
createFieldLevelEncryptionConfigResponse_httpStatus :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse Core.Int
createFieldLevelEncryptionConfigResponse_httpStatus = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {httpStatus = a} :: CreateFieldLevelEncryptionConfigResponse)

instance
  Core.NFData
    CreateFieldLevelEncryptionConfigResponse
