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
-- Module      : Amazonka.CloudFront.CreateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a field-level encryption profile.
module Amazonka.CloudFront.CreateFieldLevelEncryptionProfile
  ( -- * Creating a Request
    CreateFieldLevelEncryptionProfile (..),
    newCreateFieldLevelEncryptionProfile,

    -- * Request Lenses
    createFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,

    -- * Destructuring the Response
    CreateFieldLevelEncryptionProfileResponse (..),
    newCreateFieldLevelEncryptionProfileResponse,

    -- * Response Lenses
    createFieldLevelEncryptionProfileResponse_eTag,
    createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    createFieldLevelEncryptionProfileResponse_location,
    createFieldLevelEncryptionProfileResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFieldLevelEncryptionProfile' smart constructor.
data CreateFieldLevelEncryptionProfile = CreateFieldLevelEncryptionProfile'
  { -- | The request to create a field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFieldLevelEncryptionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionProfileConfig', 'createFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig' - The request to create a field-level encryption profile.
newCreateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  CreateFieldLevelEncryptionProfile
newCreateFieldLevelEncryptionProfile
  pFieldLevelEncryptionProfileConfig_ =
    CreateFieldLevelEncryptionProfile'
      { fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_
      }

-- | The request to create a field-level encryption profile.
createFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig :: Lens.Lens' CreateFieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
createFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig = Lens.lens (\CreateFieldLevelEncryptionProfile' {fieldLevelEncryptionProfileConfig} -> fieldLevelEncryptionProfileConfig) (\s@CreateFieldLevelEncryptionProfile' {} a -> s {fieldLevelEncryptionProfileConfig = a} :: CreateFieldLevelEncryptionProfile)

instance
  Core.AWSRequest
    CreateFieldLevelEncryptionProfile
  where
  type
    AWSResponse CreateFieldLevelEncryptionProfile =
      CreateFieldLevelEncryptionProfileResponse
  request overrides =
    Request.postXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionProfileResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (h Data..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateFieldLevelEncryptionProfile
  where
  hashWithSalt
    _salt
    CreateFieldLevelEncryptionProfile' {..} =
      _salt
        `Prelude.hashWithSalt` fieldLevelEncryptionProfileConfig

instance
  Prelude.NFData
    CreateFieldLevelEncryptionProfile
  where
  rnf CreateFieldLevelEncryptionProfile' {..} =
    Prelude.rnf fieldLevelEncryptionProfileConfig

instance
  Data.ToElement
    CreateFieldLevelEncryptionProfile
  where
  toElement CreateFieldLevelEncryptionProfile' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      fieldLevelEncryptionProfileConfig

instance
  Data.ToHeaders
    CreateFieldLevelEncryptionProfile
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateFieldLevelEncryptionProfile
  where
  toPath =
    Prelude.const
      "/2020-05-31/field-level-encryption-profile"

instance
  Data.ToQuery
    CreateFieldLevelEncryptionProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFieldLevelEncryptionProfileResponse' smart constructor.
data CreateFieldLevelEncryptionProfileResponse = CreateFieldLevelEncryptionProfileResponse'
  { -- | The current version of the field level encryption profile. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Returned when you create a new field-level encryption profile.
    fieldLevelEncryptionProfile :: Prelude.Maybe FieldLevelEncryptionProfile,
    -- | The fully qualified URI of the new profile resource just created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createFieldLevelEncryptionProfileResponse_eTag' - The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionProfile', 'createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile' - Returned when you create a new field-level encryption profile.
--
-- 'location', 'createFieldLevelEncryptionProfileResponse_location' - The fully qualified URI of the new profile resource just created.
--
-- 'httpStatus', 'createFieldLevelEncryptionProfileResponse_httpStatus' - The response's http status code.
newCreateFieldLevelEncryptionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFieldLevelEncryptionProfileResponse
newCreateFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    CreateFieldLevelEncryptionProfileResponse'
      { eTag =
          Prelude.Nothing,
        fieldLevelEncryptionProfile =
          Prelude.Nothing,
        location = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
createFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
createFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | Returned when you create a new field-level encryption profile.
createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Prelude.Maybe FieldLevelEncryptionProfile)
createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | The fully qualified URI of the new profile resource just created.
createFieldLevelEncryptionProfileResponse_location :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
createFieldLevelEncryptionProfileResponse_location = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {location} -> location) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {location = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
createFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse Prelude.Int
createFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: CreateFieldLevelEncryptionProfileResponse)

instance
  Prelude.NFData
    CreateFieldLevelEncryptionProfileResponse
  where
  rnf CreateFieldLevelEncryptionProfileResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf fieldLevelEncryptionProfile
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
