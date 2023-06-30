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
-- Module      : Amazonka.WorkMail.PutMobileDeviceAccessOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a mobile device access override for the given
-- WorkMail organization, user, and device.
module Amazonka.WorkMail.PutMobileDeviceAccessOverride
  ( -- * Creating a Request
    PutMobileDeviceAccessOverride (..),
    newPutMobileDeviceAccessOverride,

    -- * Request Lenses
    putMobileDeviceAccessOverride_description,
    putMobileDeviceAccessOverride_organizationId,
    putMobileDeviceAccessOverride_userId,
    putMobileDeviceAccessOverride_deviceId,
    putMobileDeviceAccessOverride_effect,

    -- * Destructuring the Response
    PutMobileDeviceAccessOverrideResponse (..),
    newPutMobileDeviceAccessOverrideResponse,

    -- * Response Lenses
    putMobileDeviceAccessOverrideResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newPutMobileDeviceAccessOverride' smart constructor.
data PutMobileDeviceAccessOverride = PutMobileDeviceAccessOverride'
  { -- | A description of the override.
    description :: Prelude.Maybe Prelude.Text,
    -- | Identifies the WorkMail organization for which you create the override.
    organizationId :: Prelude.Text,
    -- | The WorkMail user for which you create the override. Accepts the
    -- following types of user identities:
    --
    -- -   User ID: @12345678-1234-1234-1234-123456789012@ or
    --     @S-1-1-12-1234567890-123456789-123456789-1234@
    --
    -- -   Email address: @user\@domain.tld@
    --
    -- -   User name: @user@
    userId :: Prelude.Text,
    -- | The mobile device for which you create the override. @DeviceId@ is case
    -- insensitive.
    deviceId :: Prelude.Text,
    -- | The effect of the override, @ALLOW@ or @DENY@.
    effect :: MobileDeviceAccessRuleEffect
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMobileDeviceAccessOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'putMobileDeviceAccessOverride_description' - A description of the override.
--
-- 'organizationId', 'putMobileDeviceAccessOverride_organizationId' - Identifies the WorkMail organization for which you create the override.
--
-- 'userId', 'putMobileDeviceAccessOverride_userId' - The WorkMail user for which you create the override. Accepts the
-- following types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
--
-- 'deviceId', 'putMobileDeviceAccessOverride_deviceId' - The mobile device for which you create the override. @DeviceId@ is case
-- insensitive.
--
-- 'effect', 'putMobileDeviceAccessOverride_effect' - The effect of the override, @ALLOW@ or @DENY@.
newPutMobileDeviceAccessOverride ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'effect'
  MobileDeviceAccessRuleEffect ->
  PutMobileDeviceAccessOverride
newPutMobileDeviceAccessOverride
  pOrganizationId_
  pUserId_
  pDeviceId_
  pEffect_ =
    PutMobileDeviceAccessOverride'
      { description =
          Prelude.Nothing,
        organizationId = pOrganizationId_,
        userId = pUserId_,
        deviceId = pDeviceId_,
        effect = pEffect_
      }

-- | A description of the override.
putMobileDeviceAccessOverride_description :: Lens.Lens' PutMobileDeviceAccessOverride (Prelude.Maybe Prelude.Text)
putMobileDeviceAccessOverride_description = Lens.lens (\PutMobileDeviceAccessOverride' {description} -> description) (\s@PutMobileDeviceAccessOverride' {} a -> s {description = a} :: PutMobileDeviceAccessOverride)

-- | Identifies the WorkMail organization for which you create the override.
putMobileDeviceAccessOverride_organizationId :: Lens.Lens' PutMobileDeviceAccessOverride Prelude.Text
putMobileDeviceAccessOverride_organizationId = Lens.lens (\PutMobileDeviceAccessOverride' {organizationId} -> organizationId) (\s@PutMobileDeviceAccessOverride' {} a -> s {organizationId = a} :: PutMobileDeviceAccessOverride)

-- | The WorkMail user for which you create the override. Accepts the
-- following types of user identities:
--
-- -   User ID: @12345678-1234-1234-1234-123456789012@ or
--     @S-1-1-12-1234567890-123456789-123456789-1234@
--
-- -   Email address: @user\@domain.tld@
--
-- -   User name: @user@
putMobileDeviceAccessOverride_userId :: Lens.Lens' PutMobileDeviceAccessOverride Prelude.Text
putMobileDeviceAccessOverride_userId = Lens.lens (\PutMobileDeviceAccessOverride' {userId} -> userId) (\s@PutMobileDeviceAccessOverride' {} a -> s {userId = a} :: PutMobileDeviceAccessOverride)

-- | The mobile device for which you create the override. @DeviceId@ is case
-- insensitive.
putMobileDeviceAccessOverride_deviceId :: Lens.Lens' PutMobileDeviceAccessOverride Prelude.Text
putMobileDeviceAccessOverride_deviceId = Lens.lens (\PutMobileDeviceAccessOverride' {deviceId} -> deviceId) (\s@PutMobileDeviceAccessOverride' {} a -> s {deviceId = a} :: PutMobileDeviceAccessOverride)

-- | The effect of the override, @ALLOW@ or @DENY@.
putMobileDeviceAccessOverride_effect :: Lens.Lens' PutMobileDeviceAccessOverride MobileDeviceAccessRuleEffect
putMobileDeviceAccessOverride_effect = Lens.lens (\PutMobileDeviceAccessOverride' {effect} -> effect) (\s@PutMobileDeviceAccessOverride' {} a -> s {effect = a} :: PutMobileDeviceAccessOverride)

instance
  Core.AWSRequest
    PutMobileDeviceAccessOverride
  where
  type
    AWSResponse PutMobileDeviceAccessOverride =
      PutMobileDeviceAccessOverrideResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutMobileDeviceAccessOverrideResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutMobileDeviceAccessOverride
  where
  hashWithSalt _salt PutMobileDeviceAccessOverride' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` effect

instance Prelude.NFData PutMobileDeviceAccessOverride where
  rnf PutMobileDeviceAccessOverride' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf effect

instance Data.ToHeaders PutMobileDeviceAccessOverride where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.PutMobileDeviceAccessOverride" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutMobileDeviceAccessOverride where
  toJSON PutMobileDeviceAccessOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("UserId" Data..= userId),
            Prelude.Just ("DeviceId" Data..= deviceId),
            Prelude.Just ("Effect" Data..= effect)
          ]
      )

instance Data.ToPath PutMobileDeviceAccessOverride where
  toPath = Prelude.const "/"

instance Data.ToQuery PutMobileDeviceAccessOverride where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMobileDeviceAccessOverrideResponse' smart constructor.
data PutMobileDeviceAccessOverrideResponse = PutMobileDeviceAccessOverrideResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMobileDeviceAccessOverrideResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putMobileDeviceAccessOverrideResponse_httpStatus' - The response's http status code.
newPutMobileDeviceAccessOverrideResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutMobileDeviceAccessOverrideResponse
newPutMobileDeviceAccessOverrideResponse pHttpStatus_ =
  PutMobileDeviceAccessOverrideResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putMobileDeviceAccessOverrideResponse_httpStatus :: Lens.Lens' PutMobileDeviceAccessOverrideResponse Prelude.Int
putMobileDeviceAccessOverrideResponse_httpStatus = Lens.lens (\PutMobileDeviceAccessOverrideResponse' {httpStatus} -> httpStatus) (\s@PutMobileDeviceAccessOverrideResponse' {} a -> s {httpStatus = a} :: PutMobileDeviceAccessOverrideResponse)

instance
  Prelude.NFData
    PutMobileDeviceAccessOverrideResponse
  where
  rnf PutMobileDeviceAccessOverrideResponse' {..} =
    Prelude.rnf httpStatus
