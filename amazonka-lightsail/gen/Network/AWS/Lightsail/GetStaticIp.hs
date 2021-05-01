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
-- Module      : Network.AWS.Lightsail.GetStaticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific static IP.
module Network.AWS.Lightsail.GetStaticIp
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStaticIp' smart constructor.
data GetStaticIp = GetStaticIp'
  { -- | The name of the static IP in Lightsail.
    staticIpName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetStaticIp where
  type Rs GetStaticIp = GetStaticIpResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStaticIpResponse'
            Prelude.<$> (x Prelude..?> "staticIp")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStaticIp

instance Prelude.NFData GetStaticIp

instance Prelude.ToHeaders GetStaticIp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetStaticIp" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetStaticIp where
  toJSON GetStaticIp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("staticIpName" Prelude..= staticIpName)
          ]
      )

instance Prelude.ToPath GetStaticIp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetStaticIp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStaticIpResponse' smart constructor.
data GetStaticIpResponse = GetStaticIpResponse'
  { -- | An array of key-value pairs containing information about the requested
    -- static IP.
    staticIp :: Prelude.Maybe StaticIp,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetStaticIpResponse
