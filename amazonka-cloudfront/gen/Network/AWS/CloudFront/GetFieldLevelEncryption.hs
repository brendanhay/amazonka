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
-- Module      : Network.AWS.CloudFront.GetFieldLevelEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the field-level encryption configuration information.
module Network.AWS.CloudFront.GetFieldLevelEncryption
  ( -- * Creating a Request
    GetFieldLevelEncryption (..),
    newGetFieldLevelEncryption,

    -- * Request Lenses
    getFieldLevelEncryption_id,

    -- * Destructuring the Response
    GetFieldLevelEncryptionResponse (..),
    newGetFieldLevelEncryptionResponse,

    -- * Response Lenses
    getFieldLevelEncryptionResponse_eTag,
    getFieldLevelEncryptionResponse_fieldLevelEncryption,
    getFieldLevelEncryptionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFieldLevelEncryption' smart constructor.
data GetFieldLevelEncryption = GetFieldLevelEncryption'
  { -- | Request the ID for the field-level encryption configuration information.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFieldLevelEncryption_id' - Request the ID for the field-level encryption configuration information.
newGetFieldLevelEncryption ::
  -- | 'id'
  Core.Text ->
  GetFieldLevelEncryption
newGetFieldLevelEncryption pId_ =
  GetFieldLevelEncryption' {id = pId_}

-- | Request the ID for the field-level encryption configuration information.
getFieldLevelEncryption_id :: Lens.Lens' GetFieldLevelEncryption Core.Text
getFieldLevelEncryption_id = Lens.lens (\GetFieldLevelEncryption' {id} -> id) (\s@GetFieldLevelEncryption' {} a -> s {id = a} :: GetFieldLevelEncryption)

instance Core.AWSRequest GetFieldLevelEncryption where
  type
    AWSResponse GetFieldLevelEncryption =
      GetFieldLevelEncryptionResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetFieldLevelEncryptionResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetFieldLevelEncryption

instance Core.NFData GetFieldLevelEncryption

instance Core.ToHeaders GetFieldLevelEncryption where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetFieldLevelEncryption where
  toPath GetFieldLevelEncryption' {..} =
    Core.mconcat
      ["/2020-05-31/field-level-encryption/", Core.toBS id]

instance Core.ToQuery GetFieldLevelEncryption where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetFieldLevelEncryptionResponse' smart constructor.
data GetFieldLevelEncryptionResponse = GetFieldLevelEncryptionResponse'
  { -- | The current version of the field level encryption configuration. For
    -- example: @E2QWRUHAPOMQZL@.
    eTag :: Core.Maybe Core.Text,
    -- | Return the field-level encryption configuration information.
    fieldLevelEncryption :: Core.Maybe FieldLevelEncryption,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetFieldLevelEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getFieldLevelEncryptionResponse_eTag' - The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
--
-- 'fieldLevelEncryption', 'getFieldLevelEncryptionResponse_fieldLevelEncryption' - Return the field-level encryption configuration information.
--
-- 'httpStatus', 'getFieldLevelEncryptionResponse_httpStatus' - The response's http status code.
newGetFieldLevelEncryptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetFieldLevelEncryptionResponse
newGetFieldLevelEncryptionResponse pHttpStatus_ =
  GetFieldLevelEncryptionResponse'
    { eTag =
        Core.Nothing,
      fieldLevelEncryption = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the field level encryption configuration. For
-- example: @E2QWRUHAPOMQZL@.
getFieldLevelEncryptionResponse_eTag :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe Core.Text)
getFieldLevelEncryptionResponse_eTag = Lens.lens (\GetFieldLevelEncryptionResponse' {eTag} -> eTag) (\s@GetFieldLevelEncryptionResponse' {} a -> s {eTag = a} :: GetFieldLevelEncryptionResponse)

-- | Return the field-level encryption configuration information.
getFieldLevelEncryptionResponse_fieldLevelEncryption :: Lens.Lens' GetFieldLevelEncryptionResponse (Core.Maybe FieldLevelEncryption)
getFieldLevelEncryptionResponse_fieldLevelEncryption = Lens.lens (\GetFieldLevelEncryptionResponse' {fieldLevelEncryption} -> fieldLevelEncryption) (\s@GetFieldLevelEncryptionResponse' {} a -> s {fieldLevelEncryption = a} :: GetFieldLevelEncryptionResponse)

-- | The response's http status code.
getFieldLevelEncryptionResponse_httpStatus :: Lens.Lens' GetFieldLevelEncryptionResponse Core.Int
getFieldLevelEncryptionResponse_httpStatus = Lens.lens (\GetFieldLevelEncryptionResponse' {httpStatus} -> httpStatus) (\s@GetFieldLevelEncryptionResponse' {} a -> s {httpStatus = a} :: GetFieldLevelEncryptionResponse)

instance Core.NFData GetFieldLevelEncryptionResponse
