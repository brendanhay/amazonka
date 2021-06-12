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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFieldLevelEncryptionProfile' smart constructor.
data GetFieldLevelEncryptionProfile = GetFieldLevelEncryptionProfile'
  { -- | Get the ID for the field-level encryption profile information.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetFieldLevelEncryptionProfile
newGetFieldLevelEncryptionProfile pId_ =
  GetFieldLevelEncryptionProfile' {id = pId_}

-- | Get the ID for the field-level encryption profile information.
getFieldLevelEncryptionProfile_id :: Lens.Lens' GetFieldLevelEncryptionProfile Core.Text
getFieldLevelEncryptionProfile_id = Lens.lens (\GetFieldLevelEncryptionProfile' {id} -> id) (\s@GetFieldLevelEncryptionProfile' {} a -> s {id = a} :: GetFieldLevelEncryptionProfile)

instance
  Core.AWSRequest
    GetFieldLevelEncryptionProfile
  where
  type
    AWSResponse GetFieldLevelEncryptionProfile =
      GetFieldLevelEncryptionProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionProfileResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFieldLevelEncryptionProfile

instance Core.NFData GetFieldLevelEncryptionProfile

instance
  Core.ToHeaders
    GetFieldLevelEncryptionProfile
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetFieldLevelEncryptionProfile where
  toPath GetFieldLevelEncryptionProfile' {..} =
    Core.mconcat
      [ "/2020-05-31/field-level-encryption-profile/",
        Core.toBS id
      ]

instance Core.ToQuery GetFieldLevelEncryptionProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetFieldLevelEncryptionProfileResponse' smart constructor.
data GetFieldLevelEncryptionProfileResponse = GetFieldLevelEncryptionProfileResponse'
  { -- | The current version of the field level encryption profile. For example:
    -- @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | Return the field-level encryption profile information.
    fieldLevelEncryptionProfile :: Core.Maybe FieldLevelEncryptionProfile,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetFieldLevelEncryptionProfileResponse
newGetFieldLevelEncryptionProfileResponse
  pHttpStatus_ =
    GetFieldLevelEncryptionProfileResponse'
      { eTag =
          Core.Nothing,
        fieldLevelEncryptionProfile =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the field level encryption profile. For example:
-- @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionProfileResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Core.Maybe Core.Text)
getFieldLevelEncryptionProfileResponse_eTag = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionProfileResponse)

-- | Return the field-level encryption profile information.
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile :: Lens.Lens' GetFieldLevelEncryptionProfileResponse (Core.Maybe FieldLevelEncryptionProfile)
getFieldLevelEncryptionProfileResponse_fieldLevelEncryptionProfile = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {fieldLevelEncryptionProfile} -> fieldLevelEncryptionProfile) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {fieldLevelEncryptionProfile = a} :: GetFieldLevelEncryptionProfileResponse)

-- | The response's http status code.
getFieldLevelEncryptionProfileResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionProfileResponse Core.Int
getFieldLevelEncryptionProfileResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionProfileResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionProfileResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionProfileResponse)

instance
  Core.NFData
    GetFieldLevelEncryptionProfileResponse
