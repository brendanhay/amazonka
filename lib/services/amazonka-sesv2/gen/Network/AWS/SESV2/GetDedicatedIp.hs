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
-- Module      : Network.AWS.SESV2.GetDedicatedIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a dedicated IP address, including the name of the
-- dedicated IP pool that it\'s associated with, as well information about
-- the automatic warm-up process for the address.
module Network.AWS.SESV2.GetDedicatedIp
  ( -- * Creating a Request
    GetDedicatedIp (..),
    newGetDedicatedIp,

    -- * Request Lenses
    getDedicatedIp_ip,

    -- * Destructuring the Response
    GetDedicatedIpResponse (..),
    newGetDedicatedIpResponse,

    -- * Response Lenses
    getDedicatedIpResponse_dedicatedIp,
    getDedicatedIpResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESV2.Types

-- | A request to obtain more information about a dedicated IP address.
--
-- /See:/ 'newGetDedicatedIp' smart constructor.
data GetDedicatedIp = GetDedicatedIp'
  { -- | The IP address that you want to obtain more information about. The value
    -- you specify has to be a dedicated IP address that\'s assocaited with
    -- your Amazon Web Services account.
    ip :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ip', 'getDedicatedIp_ip' - The IP address that you want to obtain more information about. The value
-- you specify has to be a dedicated IP address that\'s assocaited with
-- your Amazon Web Services account.
newGetDedicatedIp ::
  -- | 'ip'
  Prelude.Text ->
  GetDedicatedIp
newGetDedicatedIp pIp_ = GetDedicatedIp' {ip = pIp_}

-- | The IP address that you want to obtain more information about. The value
-- you specify has to be a dedicated IP address that\'s assocaited with
-- your Amazon Web Services account.
getDedicatedIp_ip :: Lens.Lens' GetDedicatedIp Prelude.Text
getDedicatedIp_ip = Lens.lens (\GetDedicatedIp' {ip} -> ip) (\s@GetDedicatedIp' {} a -> s {ip = a} :: GetDedicatedIp)

instance Core.AWSRequest GetDedicatedIp where
  type
    AWSResponse GetDedicatedIp =
      GetDedicatedIpResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDedicatedIpResponse'
            Prelude.<$> (x Core..?> "DedicatedIp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDedicatedIp

instance Prelude.NFData GetDedicatedIp

instance Core.ToHeaders GetDedicatedIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDedicatedIp where
  toPath GetDedicatedIp' {..} =
    Prelude.mconcat
      ["/v2/email/dedicated-ips/", Core.toBS ip]

instance Core.ToQuery GetDedicatedIp where
  toQuery = Prelude.const Prelude.mempty

-- | Information about a dedicated IP address.
--
-- /See:/ 'newGetDedicatedIpResponse' smart constructor.
data GetDedicatedIpResponse = GetDedicatedIpResponse'
  { -- | An object that contains information about a dedicated IP address.
    dedicatedIp :: Prelude.Maybe DedicatedIp,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDedicatedIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedIp', 'getDedicatedIpResponse_dedicatedIp' - An object that contains information about a dedicated IP address.
--
-- 'httpStatus', 'getDedicatedIpResponse_httpStatus' - The response's http status code.
newGetDedicatedIpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDedicatedIpResponse
newGetDedicatedIpResponse pHttpStatus_ =
  GetDedicatedIpResponse'
    { dedicatedIp =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about a dedicated IP address.
getDedicatedIpResponse_dedicatedIp :: Lens.Lens' GetDedicatedIpResponse (Prelude.Maybe DedicatedIp)
getDedicatedIpResponse_dedicatedIp = Lens.lens (\GetDedicatedIpResponse' {dedicatedIp} -> dedicatedIp) (\s@GetDedicatedIpResponse' {} a -> s {dedicatedIp = a} :: GetDedicatedIpResponse)

-- | The response's http status code.
getDedicatedIpResponse_httpStatus :: Lens.Lens' GetDedicatedIpResponse Prelude.Int
getDedicatedIpResponse_httpStatus = Lens.lens (\GetDedicatedIpResponse' {httpStatus} -> httpStatus) (\s@GetDedicatedIpResponse' {} a -> s {httpStatus = a} :: GetDedicatedIpResponse)

instance Prelude.NFData GetDedicatedIpResponse
