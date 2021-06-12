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
-- Module      : Network.AWS.CertificateManager.PutAccountConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or modifies account-level configurations in ACM.
--
-- The supported configuration option is @DaysBeforeExpiry@. This option
-- specifies the number of days prior to certificate expiration when ACM
-- starts generating @EventBridge@ events. ACM sends one event per day per
-- certificate until the certificate expires. By default, accounts receive
-- events starting 45 days before certificate expiration.
module Network.AWS.CertificateManager.PutAccountConfiguration
  ( -- * Creating a Request
    PutAccountConfiguration (..),
    newPutAccountConfiguration,

    -- * Request Lenses
    putAccountConfiguration_expiryEvents,
    putAccountConfiguration_idempotencyToken,

    -- * Destructuring the Response
    PutAccountConfigurationResponse (..),
    newPutAccountConfigurationResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutAccountConfiguration' smart constructor.
data PutAccountConfiguration = PutAccountConfiguration'
  { -- | Specifies expiration events associated with an account.
    expiryEvents :: Core.Maybe ExpiryEventsConfiguration,
    -- | Customer-chosen string used to distinguish between calls to
    -- @PutAccountConfiguration@. Idempotency tokens time out after one hour.
    -- If you call @PutAccountConfiguration@ multiple times with the same
    -- unexpired idempotency token, ACM treats it as the same request and
    -- returns the original result. If you change the idempotency token for
    -- each call, ACM treats each call as a new request.
    idempotencyToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAccountConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryEvents', 'putAccountConfiguration_expiryEvents' - Specifies expiration events associated with an account.
--
-- 'idempotencyToken', 'putAccountConfiguration_idempotencyToken' - Customer-chosen string used to distinguish between calls to
-- @PutAccountConfiguration@. Idempotency tokens time out after one hour.
-- If you call @PutAccountConfiguration@ multiple times with the same
-- unexpired idempotency token, ACM treats it as the same request and
-- returns the original result. If you change the idempotency token for
-- each call, ACM treats each call as a new request.
newPutAccountConfiguration ::
  -- | 'idempotencyToken'
  Core.Text ->
  PutAccountConfiguration
newPutAccountConfiguration pIdempotencyToken_ =
  PutAccountConfiguration'
    { expiryEvents =
        Core.Nothing,
      idempotencyToken = pIdempotencyToken_
    }

-- | Specifies expiration events associated with an account.
putAccountConfiguration_expiryEvents :: Lens.Lens' PutAccountConfiguration (Core.Maybe ExpiryEventsConfiguration)
putAccountConfiguration_expiryEvents = Lens.lens (\PutAccountConfiguration' {expiryEvents} -> expiryEvents) (\s@PutAccountConfiguration' {} a -> s {expiryEvents = a} :: PutAccountConfiguration)

-- | Customer-chosen string used to distinguish between calls to
-- @PutAccountConfiguration@. Idempotency tokens time out after one hour.
-- If you call @PutAccountConfiguration@ multiple times with the same
-- unexpired idempotency token, ACM treats it as the same request and
-- returns the original result. If you change the idempotency token for
-- each call, ACM treats each call as a new request.
putAccountConfiguration_idempotencyToken :: Lens.Lens' PutAccountConfiguration Core.Text
putAccountConfiguration_idempotencyToken = Lens.lens (\PutAccountConfiguration' {idempotencyToken} -> idempotencyToken) (\s@PutAccountConfiguration' {} a -> s {idempotencyToken = a} :: PutAccountConfiguration)

instance Core.AWSRequest PutAccountConfiguration where
  type
    AWSResponse PutAccountConfiguration =
      PutAccountConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      PutAccountConfigurationResponse'

instance Core.Hashable PutAccountConfiguration

instance Core.NFData PutAccountConfiguration

instance Core.ToHeaders PutAccountConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.PutAccountConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAccountConfiguration where
  toJSON PutAccountConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExpiryEvents" Core..=) Core.<$> expiryEvents,
            Core.Just
              ("IdempotencyToken" Core..= idempotencyToken)
          ]
      )

instance Core.ToPath PutAccountConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutAccountConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAccountConfigurationResponse' smart constructor.
data PutAccountConfigurationResponse = PutAccountConfigurationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAccountConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutAccountConfigurationResponse ::
  PutAccountConfigurationResponse
newPutAccountConfigurationResponse =
  PutAccountConfigurationResponse'

instance Core.NFData PutAccountConfigurationResponse
