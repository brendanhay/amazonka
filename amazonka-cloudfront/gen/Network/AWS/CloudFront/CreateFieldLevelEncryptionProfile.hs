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
-- Module      : Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a field-level encryption profile.
module Network.AWS.CloudFront.CreateFieldLevelEncryptionProfile
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

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFieldLevelEncryptionProfile' smart constructor.
data CreateFieldLevelEncryptionProfile = CreateFieldLevelEncryptionProfile'
  { -- | The request to create a field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFieldLevelEncryptionProfileResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateFieldLevelEncryptionProfile

instance
  Core.NFData
    CreateFieldLevelEncryptionProfile

instance
  Core.ToElement
    CreateFieldLevelEncryptionProfile
  where
  toElement CreateFieldLevelEncryptionProfile' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      fieldLevelEncryptionProfileConfig

instance
  Core.ToHeaders
    CreateFieldLevelEncryptionProfile
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    CreateFieldLevelEncryptionProfile
  where
  toPath =
    Core.const
      "/2020-05-31/field-level-encryption-profile"

instance
  Core.ToQuery
    CreateFieldLevelEncryptionProfile
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateFieldLevelEncryptionProfileResponse' smart constructor.
data CreateFieldLevelEncryptionProfileResponse = CreateFieldLevelEncryptionProfileResponse'
  { -- | The current version of the field level encryption profile. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | Returned when you create a new field-level encryption profile.
    fieldLevelEncryptionProfile :: Core.Maybe FieldLevelEncryptionProfile,
    -- | The fully qualified URI of the new profile resource just created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateFieldLevelEncryptionProfileResponse
newCreateFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    CreateFieldLevelEncryptionProfileResponse'
      { eTag =
          Core.Nothing,
        fieldLevelEncryptionProfile =
          Core.Nothing,
        location = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
createFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe Core.Text)
createFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | Returned when you create a new field-level encryption profile.
createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe FieldLevelEncryptionProfile)
createFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | The fully qualified URI of the new profile resource just created.
createFieldLevelEncryptionProfileResponse_location :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse (Core.Maybe Core.Text)
createFieldLevelEncryptionProfileResponse_location = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {location} -> location) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {location = a} :: CreateFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
createFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' CreateFieldLevelEncryptionProfileResponse Core.Int
createFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\CreateFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@CreateFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: CreateFieldLevelEncryptionProfileResponse)

instance
  Core.NFData
    CreateFieldLevelEncryptionProfileResponse
