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
-- Module      : Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption profile.
module Network.AWS.CloudFront.UpdateFieldLevelEncryptionProfile
  ( -- * Creating a Request
    UpdateFieldLevelEncryptionProfile (..),
    newUpdateFieldLevelEncryptionProfile,

    -- * Request Lenses
    updateFieldLevelEncryptionProfile_ifMatch,
    updateFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig,
    updateFieldLevelEncryptionProfile_id,

    -- * Destructuring the Response
    UpdateFieldLevelEncryptionProfileResponse (..),
    newUpdateFieldLevelEncryptionProfileResponse,

    -- * Response Lenses
    updateFieldLevelEncryptionProfileResponse_eTag,
    updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    updateFieldLevelEncryptionProfileResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFieldLevelEncryptionProfile' smart constructor.
data UpdateFieldLevelEncryptionProfile = UpdateFieldLevelEncryptionProfile'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- profile identity to update. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | Request to update a field-level encryption profile.
    fieldLevelEncryptionProfileConfig :: FieldLevelEncryptionProfileConfig,
    -- | The ID of the field-level encryption profile request.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldLevelEncryptionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateFieldLevelEncryptionProfile_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- profile identity to update. For example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionProfileConfig', 'updateFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig' - Request to update a field-level encryption profile.
--
-- 'id', 'updateFieldLevelEncryptionProfile_id' - The ID of the field-level encryption profile request.
newUpdateFieldLevelEncryptionProfile ::
  -- | 'fieldLevelEncryptionProfileConfig'
  FieldLevelEncryptionProfileConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateFieldLevelEncryptionProfile
newUpdateFieldLevelEncryptionProfile
  pFieldLevelEncryptionProfileConfig_
  pId_ =
    UpdateFieldLevelEncryptionProfile'
      { ifMatch =
          Prelude.Nothing,
        fieldLevelEncryptionProfileConfig =
          pFieldLevelEncryptionProfileConfig_,
        id = pId_
      }

-- | The value of the @ETag@ header that you received when retrieving the
-- profile identity to update. For example: @E2QWRUHAPOMQZL@.
updateFieldLevelEncryptionProfile_ifMatch :: Lens.Lens' UpdateFieldLevelEncryptionProfile (Prelude.Maybe Prelude.Text)
updateFieldLevelEncryptionProfile_ifMatch = Lens.lens (\UpdateFieldLevelEncryptionProfile' {ifMatch} -> ifMatch) (\s@UpdateFieldLevelEncryptionProfile' {} a -> s {ifMatch = a} :: UpdateFieldLevelEncryptionProfile)

-- | Request to update a field-level encryption profile.
updateFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig :: Lens.Lens' UpdateFieldLevelEncryptionProfile FieldLevelEncryptionProfileConfig
updateFieldLevelEncryptionProfile_fieldLevelEncryptionProfileConfig = Lens.lens (\UpdateFieldLevelEncryptionProfile' {fieldLevelEncryptionProfileConfig} -> fieldLevelEncryptionProfileConfig) (\s@UpdateFieldLevelEncryptionProfile' {} a -> s {fieldLevelEncryptionProfileConfig = a} :: UpdateFieldLevelEncryptionProfile)

-- | The ID of the field-level encryption profile request.
updateFieldLevelEncryptionProfile_id :: Lens.Lens' UpdateFieldLevelEncryptionProfile Prelude.Text
updateFieldLevelEncryptionProfile_id = Lens.lens (\UpdateFieldLevelEncryptionProfile' {id} -> id) (\s@UpdateFieldLevelEncryptionProfile' {} a -> s {id = a} :: UpdateFieldLevelEncryptionProfile)

instance
  Prelude.AWSRequest
    UpdateFieldLevelEncryptionProfile
  where
  type
    Rs UpdateFieldLevelEncryptionProfile =
      UpdateFieldLevelEncryptionProfileResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionProfileResponse'
            Prelude.<$> (h Prelude..#? "ETag")
              Prelude.<*> (Prelude.parseXML x)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFieldLevelEncryptionProfile

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionProfile

instance
  Prelude.ToElement
    UpdateFieldLevelEncryptionProfile
  where
  toElement UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      fieldLevelEncryptionProfileConfig

instance
  Prelude.ToHeaders
    UpdateFieldLevelEncryptionProfile
  where
  toHeaders UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance
  Prelude.ToPath
    UpdateFieldLevelEncryptionProfile
  where
  toPath UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Prelude.toBS id,
        "/config"
      ]

instance
  Prelude.ToQuery
    UpdateFieldLevelEncryptionProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFieldLevelEncryptionProfileResponse' smart constructor.
data UpdateFieldLevelEncryptionProfileResponse = UpdateFieldLevelEncryptionProfileResponse'
  { -- | The result of the field-level encryption profile request.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Return the results of updating the profile.
    fieldLevelEncryptionProfile :: Prelude.Maybe FieldLevelEncryptionProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateFieldLevelEncryptionProfileResponse_eTag' - The result of the field-level encryption profile request.
--
-- 'fieldLevelEncryptionProfile', 'updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile' - Return the results of updating the profile.
--
-- 'httpStatus', 'updateFieldLevelEncryptionProfileResponse_httpStatus' - The response's http status code.
newUpdateFieldLevelEncryptionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFieldLevelEncryptionProfileResponse
newUpdateFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    UpdateFieldLevelEncryptionProfileResponse'
      { eTag =
          Prelude.Nothing,
        fieldLevelEncryptionProfile =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The result of the field-level encryption profile request.
updateFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
updateFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: UpdateFieldLevelEncryptionProfileResponse)

-- | Return the results of updating the profile.
updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Prelude.Maybe FieldLevelEncryptionProfile)
updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: UpdateFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
updateFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse Prelude.Int
updateFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: UpdateFieldLevelEncryptionProfileResponse)

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionProfileResponse
