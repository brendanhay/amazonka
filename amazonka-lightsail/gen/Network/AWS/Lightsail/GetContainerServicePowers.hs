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
-- Module      : Network.AWS.Lightsail.GetContainerServicePowers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of powers that can be specified for your Amazon
-- Lightsail container services.
--
-- The power specifies the amount of memory, the number of vCPUs, and the
-- base price of the container service.
module Network.AWS.Lightsail.GetContainerServicePowers
  ( -- * Creating a Request
    GetContainerServicePowers (..),
    newGetContainerServicePowers,

    -- * Destructuring the Response
    GetContainerServicePowersResponse (..),
    newGetContainerServicePowersResponse,

    -- * Response Lenses
    getContainerServicePowersResponse_powers,
    getContainerServicePowersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerServicePowers' smart constructor.
data GetContainerServicePowers = GetContainerServicePowers'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServicePowers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetContainerServicePowers ::
  GetContainerServicePowers
newGetContainerServicePowers =
  GetContainerServicePowers'

instance Core.AWSRequest GetContainerServicePowers where
  type
    AWSResponse GetContainerServicePowers =
      GetContainerServicePowersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicePowersResponse'
            Core.<$> (x Core..?> "powers" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetContainerServicePowers

instance Core.NFData GetContainerServicePowers

instance Core.ToHeaders GetContainerServicePowers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetContainerServicePowers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetContainerServicePowers where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetContainerServicePowers where
  toPath = Core.const "/"

instance Core.ToQuery GetContainerServicePowers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetContainerServicePowersResponse' smart constructor.
data GetContainerServicePowersResponse = GetContainerServicePowersResponse'
  { -- | An array of objects that describe the powers that can be specified for a
    -- container service.
    powers :: Core.Maybe [ContainerServicePower],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetContainerServicePowersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'powers', 'getContainerServicePowersResponse_powers' - An array of objects that describe the powers that can be specified for a
-- container service.
--
-- 'httpStatus', 'getContainerServicePowersResponse_httpStatus' - The response's http status code.
newGetContainerServicePowersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetContainerServicePowersResponse
newGetContainerServicePowersResponse pHttpStatus_ =
  GetContainerServicePowersResponse'
    { powers =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the powers that can be specified for a
-- container service.
getContainerServicePowersResponse_powers :: Lens.Lens' GetContainerServicePowersResponse (Core.Maybe [ContainerServicePower])
getContainerServicePowersResponse_powers = Lens.lens (\GetContainerServicePowersResponse' {powers} -> powers) (\s@GetContainerServicePowersResponse' {} a -> s {powers = a} :: GetContainerServicePowersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getContainerServicePowersResponse_httpStatus :: Lens.Lens' GetContainerServicePowersResponse Core.Int
getContainerServicePowersResponse_httpStatus = Lens.lens (\GetContainerServicePowersResponse' {httpStatus} -> httpStatus) (\s@GetContainerServicePowersResponse' {} a -> s {httpStatus = a} :: GetContainerServicePowersResponse)

instance
  Core.NFData
    GetContainerServicePowersResponse
