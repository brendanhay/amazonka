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
-- Module      : Network.AWS.CertificateManager.GetAccountConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the account configuration options associated with an AWS
-- account.
module Network.AWS.CertificateManager.GetAccountConfiguration
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

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountConfiguration' smart constructor.
data GetAccountConfiguration = GetAccountConfiguration'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountConfigurationResponse'
            Core.<$> (x Core..?> "ExpiryEvents")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAccountConfiguration

instance Core.NFData GetAccountConfiguration

instance Core.ToHeaders GetAccountConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CertificateManager.GetAccountConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetAccountConfiguration where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetAccountConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery GetAccountConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAccountConfigurationResponse' smart constructor.
data GetAccountConfigurationResponse = GetAccountConfigurationResponse'
  { -- | Expiration events configuration options associated with the AWS account.
    expiryEvents :: Core.Maybe ExpiryEventsConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryEvents', 'getAccountConfigurationResponse_expiryEvents' - Expiration events configuration options associated with the AWS account.
--
-- 'httpStatus', 'getAccountConfigurationResponse_httpStatus' - The response's http status code.
newGetAccountConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAccountConfigurationResponse
newGetAccountConfigurationResponse pHttpStatus_ =
  GetAccountConfigurationResponse'
    { expiryEvents =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Expiration events configuration options associated with the AWS account.
getAccountConfigurationResponse_expiryEvents :: Lens.Lens' GetAccountConfigurationResponse (Core.Maybe ExpiryEventsConfiguration)
getAccountConfigurationResponse_expiryEvents = Lens.lens (\GetAccountConfigurationResponse' {expiryEvents} -> expiryEvents) (\s@GetAccountConfigurationResponse' {} a -> s {expiryEvents = a} :: GetAccountConfigurationResponse)

-- | The response's http status code.
getAccountConfigurationResponse_httpStatus :: Lens.Lens' GetAccountConfigurationResponse Core.Int
getAccountConfigurationResponse_httpStatus = Lens.lens (\GetAccountConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAccountConfigurationResponse' {} a -> s {httpStatus = a} :: GetAccountConfigurationResponse)

instance Core.NFData GetAccountConfigurationResponse
