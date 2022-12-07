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
-- Module      : Amazonka.CloudFront.UpdateFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a field-level encryption profile.
module Amazonka.CloudFront.UpdateFieldLevelEncryptionProfile
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
    updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    updateFieldLevelEncryptionProfileResponse_eTag,
    updateFieldLevelEncryptionProfileResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    UpdateFieldLevelEncryptionProfile
  where
  type
    AWSResponse UpdateFieldLevelEncryptionProfile =
      UpdateFieldLevelEncryptionProfileResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateFieldLevelEncryptionProfileResponse'
            Prelude.<$> (Data.parseXML x) Prelude.<*> (h Data..#? "ETag")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateFieldLevelEncryptionProfile
  where
  hashWithSalt
    _salt
    UpdateFieldLevelEncryptionProfile' {..} =
      _salt `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` fieldLevelEncryptionProfileConfig
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionProfile
  where
  rnf UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf fieldLevelEncryptionProfileConfig
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToElement
    UpdateFieldLevelEncryptionProfile
  where
  toElement UpdateFieldLevelEncryptionProfile' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}FieldLevelEncryptionProfileConfig"
      fieldLevelEncryptionProfileConfig

instance
  Data.ToHeaders
    UpdateFieldLevelEncryptionProfile
  where
  toHeaders UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance
  Data.ToPath
    UpdateFieldLevelEncryptionProfile
  where
  toPath UpdateFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Data.toBS id,
        "/config"
      ]

instance
  Data.ToQuery
    UpdateFieldLevelEncryptionProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFieldLevelEncryptionProfileResponse' smart constructor.
data UpdateFieldLevelEncryptionProfileResponse = UpdateFieldLevelEncryptionProfileResponse'
  { -- | Return the results of updating the profile.
    fieldLevelEncryptionProfile :: Prelude.Maybe FieldLevelEncryptionProfile,
    -- | The result of the field-level encryption profile request.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionProfile', 'updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile' - Return the results of updating the profile.
--
-- 'eTag', 'updateFieldLevelEncryptionProfileResponse_eTag' - The result of the field-level encryption profile request.
--
-- 'httpStatus', 'updateFieldLevelEncryptionProfileResponse_httpStatus' - The response's http status code.
newUpdateFieldLevelEncryptionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFieldLevelEncryptionProfileResponse
newUpdateFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    UpdateFieldLevelEncryptionProfileResponse'
      { fieldLevelEncryptionProfile =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Return the results of updating the profile.
updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Prelude.Maybe FieldLevelEncryptionProfile)
updateFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: UpdateFieldLevelEncryptionProfileResponse)

-- | The result of the field-level encryption profile request.
updateFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
updateFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: UpdateFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
updateFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' UpdateFieldLevelEncryptionProfileResponse Prelude.Int
updateFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\UpdateFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: UpdateFieldLevelEncryptionProfileResponse)

instance
  Prelude.NFData
    UpdateFieldLevelEncryptionProfileResponse
  where
  rnf UpdateFieldLevelEncryptionProfileResponse' {..} =
    Prelude.rnf fieldLevelEncryptionProfile
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
