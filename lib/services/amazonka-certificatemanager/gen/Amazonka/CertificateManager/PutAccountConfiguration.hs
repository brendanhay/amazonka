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
-- Module      : Amazonka.CertificateManager.PutAccountConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CertificateManager.PutAccountConfiguration
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

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutAccountConfiguration' smart constructor.
data PutAccountConfiguration = PutAccountConfiguration'
  { -- | Specifies expiration events associated with an account.
    expiryEvents :: Prelude.Maybe ExpiryEventsConfiguration,
    -- | Customer-chosen string used to distinguish between calls to
    -- @PutAccountConfiguration@. Idempotency tokens time out after one hour.
    -- If you call @PutAccountConfiguration@ multiple times with the same
    -- unexpired idempotency token, ACM treats it as the same request and
    -- returns the original result. If you change the idempotency token for
    -- each call, ACM treats each call as a new request.
    idempotencyToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  PutAccountConfiguration
newPutAccountConfiguration pIdempotencyToken_ =
  PutAccountConfiguration'
    { expiryEvents =
        Prelude.Nothing,
      idempotencyToken = pIdempotencyToken_
    }

-- | Specifies expiration events associated with an account.
putAccountConfiguration_expiryEvents :: Lens.Lens' PutAccountConfiguration (Prelude.Maybe ExpiryEventsConfiguration)
putAccountConfiguration_expiryEvents = Lens.lens (\PutAccountConfiguration' {expiryEvents} -> expiryEvents) (\s@PutAccountConfiguration' {} a -> s {expiryEvents = a} :: PutAccountConfiguration)

-- | Customer-chosen string used to distinguish between calls to
-- @PutAccountConfiguration@. Idempotency tokens time out after one hour.
-- If you call @PutAccountConfiguration@ multiple times with the same
-- unexpired idempotency token, ACM treats it as the same request and
-- returns the original result. If you change the idempotency token for
-- each call, ACM treats each call as a new request.
putAccountConfiguration_idempotencyToken :: Lens.Lens' PutAccountConfiguration Prelude.Text
putAccountConfiguration_idempotencyToken = Lens.lens (\PutAccountConfiguration' {idempotencyToken} -> idempotencyToken) (\s@PutAccountConfiguration' {} a -> s {idempotencyToken = a} :: PutAccountConfiguration)

instance Core.AWSRequest PutAccountConfiguration where
  type
    AWSResponse PutAccountConfiguration =
      PutAccountConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutAccountConfigurationResponse'

instance Prelude.Hashable PutAccountConfiguration where
  hashWithSalt _salt PutAccountConfiguration' {..} =
    _salt `Prelude.hashWithSalt` expiryEvents
      `Prelude.hashWithSalt` idempotencyToken

instance Prelude.NFData PutAccountConfiguration where
  rnf PutAccountConfiguration' {..} =
    Prelude.rnf expiryEvents
      `Prelude.seq` Prelude.rnf idempotencyToken

instance Data.ToHeaders PutAccountConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.PutAccountConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountConfiguration where
  toJSON PutAccountConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpiryEvents" Data..=) Prelude.<$> expiryEvents,
            Prelude.Just
              ("IdempotencyToken" Data..= idempotencyToken)
          ]
      )

instance Data.ToPath PutAccountConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAccountConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAccountConfigurationResponse' smart constructor.
data PutAccountConfigurationResponse = PutAccountConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutAccountConfigurationResponse ::
  PutAccountConfigurationResponse
newPutAccountConfigurationResponse =
  PutAccountConfigurationResponse'

instance
  Prelude.NFData
    PutAccountConfigurationResponse
  where
  rnf _ = ()
