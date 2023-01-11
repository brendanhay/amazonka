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
-- Module      : Amazonka.CertificateManager.GetAccountConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the account configuration options associated with an Amazon Web
-- Services account.
module Amazonka.CertificateManager.GetAccountConfiguration
  ( -- * Creating a Request
    GetAccountConfiguration (..),
    newGetAccountConfiguration,

    -- * Destructuring the Response
    GetAccountConfigurationResponse (..),
    newGetAccountConfigurationResponse,

    -- * Response Lenses
    getAccountConfigurationResponse_expiryEvents,
    getAccountConfigurationResponse_httpStatus,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountConfiguration' smart constructor.
data GetAccountConfiguration = GetAccountConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountConfiguration ::
  GetAccountConfiguration
newGetAccountConfiguration = GetAccountConfiguration'

instance Core.AWSRequest GetAccountConfiguration where
  type
    AWSResponse GetAccountConfiguration =
      GetAccountConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountConfigurationResponse'
            Prelude.<$> (x Data..?> "ExpiryEvents")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountConfiguration where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountConfiguration where
  rnf _ = ()

instance Data.ToHeaders GetAccountConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.GetAccountConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAccountConfiguration where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetAccountConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccountConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountConfigurationResponse' smart constructor.
data GetAccountConfigurationResponse = GetAccountConfigurationResponse'
  { -- | Expiration events configuration options associated with the Amazon Web
    -- Services account.
    expiryEvents :: Prelude.Maybe ExpiryEventsConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryEvents', 'getAccountConfigurationResponse_expiryEvents' - Expiration events configuration options associated with the Amazon Web
-- Services account.
--
-- 'httpStatus', 'getAccountConfigurationResponse_httpStatus' - The response's http status code.
newGetAccountConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountConfigurationResponse
newGetAccountConfigurationResponse pHttpStatus_ =
  GetAccountConfigurationResponse'
    { expiryEvents =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Expiration events configuration options associated with the Amazon Web
-- Services account.
getAccountConfigurationResponse_expiryEvents :: Lens.Lens' GetAccountConfigurationResponse (Prelude.Maybe ExpiryEventsConfiguration)
getAccountConfigurationResponse_expiryEvents = Lens.lens (\GetAccountConfigurationResponse' {expiryEvents} -> expiryEvents) (\s@GetAccountConfigurationResponse' {} a -> s {expiryEvents = a} :: GetAccountConfigurationResponse)

-- | The response's http status code.
getAccountConfigurationResponse_httpStatus :: Lens.Lens' GetAccountConfigurationResponse Prelude.Int
getAccountConfigurationResponse_httpStatus = Lens.lens (\GetAccountConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAccountConfigurationResponse' {} a -> s {httpStatus = a} :: GetAccountConfigurationResponse)

instance
  Prelude.NFData
    GetAccountConfigurationResponse
  where
  rnf GetAccountConfigurationResponse' {..} =
    Prelude.rnf expiryEvents
      `Prelude.seq` Prelude.rnf httpStatus
