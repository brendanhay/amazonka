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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption profile information.
module Network.AWS.CloudFront.GetFieldLevelEncryptionProfile
  ( -- * Creating a Request
    GetFieldLevelEncryptionProfile (..),
    newGetFieldLevelEncryptionProfile,

    -- * Request Lenses
    getFieldLevelEncryptionProfile_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionProfileResponse (..),
    newGetFieldLevelEncryptionProfileResponse,

    -- * Response Lenses
    getFieldLevelEncryptionProfileResponse_eTag,
    getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile,
    getFieldLevelEncryptionProfileResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFieldLevelEncryptionProfile' smart constructor.
data GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { -- | Get the ID for the field-level encryption profile information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    GetFieldLevelEncryptionProfile
  where
  type
    Rs GetFieldLevelEncryptionProfile =
      GetFieldLevelEncryptionProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileResponse'
            Prelude.<$> (h Prelude..#? "ETag")
            Prelude.<*> (Prelude.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetFieldLevelEncryptionProfile

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfile

instance
  Prelude.ToHeaders
    GetFieldLevelEncryptionProfile
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    GetFieldLevelEncryptionProfile
  where
  toPath GetFieldLevelEncryptionProfile' {..} =
    Prelude.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Prelude.toBS id
      ]

instance
  Prelude.ToQuery
    GetFieldLevelEncryptionProfile
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
  { -- | The current version of the field level encryption profile. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Return the field-level encryption profile information.
    fieldLevelEncryptionProfile :: Prelude.Maybe FieldLevelEncryptionProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getFieldLevelEncryptionProfileResponse_eTag' - The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryptionProfile', 'getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile' - Return the field-level encryption profile information.
--
-- 'httpStatus', 'getFieldLevelEncryptionProfileResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFieldLevelEncryptionProfileResponse
newGetFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    GetFieldLevelEncryptionProfileResponse'
      { eTag =
          Prelude.Nothing,
        fieldLevelEncryptionProfile =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Prelude.Maybe Prelude.Text)
getFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionProfileResponse)

-- | Return the field-level encryption profile information.
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Prelude.Maybe FieldLevelEncryptionProfile)
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: GetFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
getFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionProfileResponse Prelude.Int
getFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionProfileResponse)

instance
  Prelude.NFData
    GetFieldLevelEncryptionProfileResponse
