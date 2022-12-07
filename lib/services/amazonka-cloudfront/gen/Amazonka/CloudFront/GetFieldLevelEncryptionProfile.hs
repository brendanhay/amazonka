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
-- Module      : Amazonka.CloudFront.GetFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile information.
module Amazonka.CloudFront.GetFieldLevelEncryptionProfile
  ( -- * Creating a Request
    GetFieldLevelEncryptionProfile (..),
    newGetFieldLevelEncryptionProfile,

    -- * Request Lenses
    getFieldLevelEncryptionProfile_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionProfileResponse (..),
    newGetFieldLevelEncryptionProfileResponse,

    -- * Response Lenses
    getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    getFieldLevelEncryptionProfileResponse_eTag,
    getFieldLevelEncryptionProfileResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFieldLevelEncryptionProfile' smart constructor.
data GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { -- | Get the ID for the field-level encryption profile information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFieldLevelEncryptionProfile_id' - Get the ID for the field-level encryption profile information.
newGetFieldLevelEncryptionProfile ::
  -- | 'id'
  Prelude.Text ->
  GetFieldLevelEncryptionProfile
newGetFieldLevelEncryptionProfile pId_ =
  GetFieldLevelEncryptionProfile' {id = pId_}

-- | Get the ID for the field-level encryption profile information.
getFieldLevelEncryptionProfile_id :: Lens.Lens' GetFieldLevelEncryptionProfile Prelude.Text
getFieldLevelEncryptionProfile_id = Lens.lens (\GetFieldLevelEncryptionProfile' {id} -> id) (\s@GetFieldLevelEncryptionProfile' {} a -> s {id = a} :: GetFieldLevelEncryptionProfile)

instance
  Core.AWSRequest
    GetFieldLevelEncryptionProfile
  where
  type
    AWSResponse GetFieldLevelEncryptionProfile =
      GetFieldLevelEncryptionProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFieldLevelEncryptionProfile
  where
  hashWithSalt
    _salt
    GetFieldLevelEncryptionProfile' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfile
  where
  rnf GetFieldLevelEncryptionProfile' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetFieldLevelEncryptionProfile
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetFieldLevelEncryptionProfile where
  toPath GetFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Data.toBS id
      ]

instance Data.ToQuery GetFieldLevelEncryptionProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
  { -- | Return the field-level encryption profile information.
    fieldLevelEncryptionProfile :: Prelude.Maybe FieldLevelEncryptionProfile,
    -- | The current version of the field level encryption profile. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldLevelEncryptionProfile', 'getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile' - Return the field-level encryption profile information.
--
-- 'eTag', 'getFieldLevelEncryptionProfileResponse_eTag' - The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'getFieldLevelEncryptionProfileResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFieldLevelEncryptionProfileResponse
newGetFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    GetFieldLevelEncryptionProfileResponse'
      { fieldLevelEncryptionProfile =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Return the field-level encryption profile information.
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Prelude.Maybe FieldLevelEncryptionProfile)
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: GetFieldLevelEncryptionProfileResponse)

-- | The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
getFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
getFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionProfileResponse Prelude.Int
getFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionProfileResponse)

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileResponse
  where
  rnf GetFieldLevelEncryptionProfileResponse' {..} =
    Prelude.rnf fieldLevelEncryptionProfile
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
