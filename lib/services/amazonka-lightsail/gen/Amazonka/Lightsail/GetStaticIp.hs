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
-- Module      : Amazonka.Lightsail.GetStaticIp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lightsail static IP.
module Amazonka.Lightsail.GetStaticIp
  ( -- * Creating a Request
    GetStaticIp (..),
    newGetStaticIp,

    -- * Request Lenses
    getStaticIp_staticIpName,

    -- * Destructuring the Response
    GetStaticIpResponse (..),
    newGetStaticIpResponse,

    -- * Response Lenses
    getStaticIpResponse_staticIp,
    getStaticIpResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStaticIp' smart constructor.
data GetStaticIp = GetStaticIp'
  { -- | The name of the static IP in Lightsail.
    staticIpName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStaticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIpName', 'getStaticIp_staticIpName' - The name of the static IP in Lightsail.
newGetStaticIp ::
  -- | 'staticIpName'
  Prelude.Text ->
  GetStaticIp
newGetStaticIp pStaticIpName_ =
  GetStaticIp' {staticIpName = pStaticIpName_}

-- | The name of the static IP in Lightsail.
getStaticIp_staticIpName :: Lens.Lens' GetStaticIp Prelude.Text
getStaticIp_staticIpName = Lens.lens (\GetStaticIp' {staticIpName} -> staticIpName) (\s@GetStaticIp' {} a -> s {staticIpName = a} :: GetStaticIp)

instance Core.AWSRequest GetStaticIp where
  type AWSResponse GetStaticIp = GetStaticIpResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStaticIpResponse'
            Prelude.<$> (x Core..?> "staticIp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStaticIp where
  hashWithSalt _salt GetStaticIp' {..} =
    _salt `Prelude.hashWithSalt` staticIpName

instance Prelude.NFData GetStaticIp where
  rnf GetStaticIp' {..} = Prelude.rnf staticIpName

instance Core.ToHeaders GetStaticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetStaticIp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetStaticIp where
  toJSON GetStaticIp' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("staticIpName" Core..= staticIpName)]
      )

instance Core.ToPath GetStaticIp where
  toPath = Prelude.const "/"

instance Core.ToQuery GetStaticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStaticIpResponse' smart constructor.
data GetStaticIpResponse = GetStaticIpResponse'
  { -- | An array of key-value pairs containing information about the requested
    -- static IP.
    staticIp :: Prelude.Maybe StaticIp,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStaticIpResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticIp', 'getStaticIpResponse_staticIp' - An array of key-value pairs containing information about the requested
-- static IP.
--
-- 'httpStatus', 'getStaticIpResponse_httpStatus' - The response's http status code.
newGetStaticIpResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStaticIpResponse
newGetStaticIpResponse pHttpStatus_ =
  GetStaticIpResponse'
    { staticIp = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the requested
-- static IP.
getStaticIpResponse_staticIp :: Lens.Lens' GetStaticIpResponse (Prelude.Maybe StaticIp)
getStaticIpResponse_staticIp = Lens.lens (\GetStaticIpResponse' {staticIp} -> staticIp) (\s@GetStaticIpResponse' {} a -> s {staticIp = a} :: GetStaticIpResponse)

-- | The response's http status code.
getStaticIpResponse_httpStatus :: Lens.Lens' GetStaticIpResponse Prelude.Int
getStaticIpResponse_httpStatus = Lens.lens (\GetStaticIpResponse' {httpStatus} -> httpStatus) (\s@GetStaticIpResponse' {} a -> s {httpStatus = a} :: GetStaticIpResponse)

instance Prelude.NFData GetStaticIpResponse where
  rnf GetStaticIpResponse' {..} =
    Prelude.rnf staticIp
      `Prelude.seq` Prelude.rnf httpStatus
