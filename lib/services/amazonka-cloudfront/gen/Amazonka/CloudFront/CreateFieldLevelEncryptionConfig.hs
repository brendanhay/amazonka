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
-- Module      : Amazonka.CloudFront.CreateFieldLevelEncryptionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new field-level encryption configuration.
module Amazonka.CloudFront.CreateFieldLevelEncryptionConfig
  ( -- * Creating a Request
    CreateFieldLevelEncryptionConfig (..),
    newCreateFieldLevelEncryptionConfig,

    -- * Request Lenses
    createFieldLevelEncryptionConfig_fieldLevelEncryptionConfig,

    -- * Destructuring the Response
    CreateFieldLevelEncryptionConfigResponse (..),
    newCreateFieldLevelEncryptionConfigResponse,

    -- * Response Lenses
    createFieldLevelEncryptionConfigResponse_location,
    createFieldLevelEncryptionConfigResponse_eTag,
    createFieldLevelEncryptionConfigResponse_fieldLevelEncryption,
    createFieldLevelEncryptionConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFieldLevelEncryptionConfig' smart constructor.
data CreateFieldLevelEncryptionConfig = CreateFieldLevelEncryptionConfig'
  { -- | The request to create a new field-level encryption configuration.
    fieldLevelEncryptionConfig :: FieldLevelEncryptionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionConfigResponse'
            Prelude.<$> (h Core..#? "Location")
            Prelude.<*> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateFieldLevelEncryptionConfig
  where
  hashWithSalt
    _salt
    CreateFieldLevelEncryptionConfig' {..} =
      _salt
        `Prelude.hashWithSalt` fieldLevelEncryptionConfig

instance
  Prelude.NFData
    CreateFieldLevelEncryptionConfig
  where
  rnf CreateFieldLevelEncryptionConfig' {..} =
    Prelude.rnf fieldLevelEncryptionConfig

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
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateFieldLevelEncryptionConfig where
  toPath =
    Prelude.const "/2020-05-31/field-level-encryption"

instance
  Core.ToQuery
    CreateFieldLevelEncryptionConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFieldLevelEncryptionConfigResponse' smart constructor.
data CreateFieldLevelEncryptionConfigResponse = CreateFieldLevelEncryptionConfigResponse'
  { -- | The fully qualified URI of the new configuration resource just created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The current version of the field level encryption configuration. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Returned when you create a new field-level encryption configuration.
    fieldLevelEncryption :: Prelude.Maybe FieldLevelEncryption,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFieldLevelEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createFieldLevelEncryptionConfigResponse_location' - The fully qualified URI of the new configuration resource just created.
--
-- 'eTag', 'createFieldLevelEncryptionConfigResponse_eTag' - The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryption', 'createFieldLevelEncryptionConfigResponse_fieldLevelEncryption' - Returned when you create a new field-level encryption configuration.
--
-- 'httpStatus', 'createFieldLevelEncryptionConfigResponse_httpStatus' - The response's http status code.
newCreateFieldLevelEncryptionConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFieldLevelEncryptionConfigResponse
newCreateFieldLevelEncryptionConfigResponse
  pHttpStatus_ =
    CreateFieldLevelEncryptionConfigResponse'
      { location =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        fieldLevelEncryption =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The fully qualified URI of the new configuration resource just created.
createFieldLevelEncryptionConfigResponse_location :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Prelude.Maybe Prelude.Text)
createFieldLevelEncryptionConfigResponse_location = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {location} -> location) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {location = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
createFieldLevelEncryptionConfigResponse_eTag :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Prelude.Maybe Prelude.Text)
createFieldLevelEncryptionConfigResponse_eTag = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {eTag} -> eTag) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {eTag = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | Returned when you create a new field-level encryption configuration.
createFieldLevelEncryptionConfigResponse_fieldLevelEncryption :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse (Prelude.Maybe FieldLevelEncryption)
createFieldLevelEncryptionConfigResponse_fieldLevelEncryption = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {fieldLevelEncryption} -> fieldLevelEncryption) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {fieldLevelEncryption = a} :: CreateFieldLevelEncryptionConfigResponse)

-- | The response's http status code.
createFieldLevelEncryptionConfigResponse_httpStatus :: Lens.Lens' CreateFieldLevelEncryptionConfigResponse Prelude.Int
createFieldLevelEncryptionConfigResponse_httpStatus = Lens.lens (\CreateFieldLevelEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@CreateFieldLevelEncryptionConfigResponse' {} a -> s {httpStatus = a} :: CreateFieldLevelEncryptionConfigResponse)

instance
  Prelude.NFData
    CreateFieldLevelEncryptionConfigResponse
  where
  rnf CreateFieldLevelEncryptionConfigResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf fieldLevelEncryption
      `Prelude.seq` Prelude.rnf httpStatus
