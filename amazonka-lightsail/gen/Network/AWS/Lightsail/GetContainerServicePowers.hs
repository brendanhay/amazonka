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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetContainerServicePowers' smart constructor.
data GetContainerServicePowers = GetContainerServicePowers'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetContainerServicePowers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetContainerServicePowers ::
  GetContainerServicePowers
newGetContainerServicePowers =
  GetContainerServicePowers'

instance Prelude.AWSRequest GetContainerServicePowers where
  type
    Rs GetContainerServicePowers =
      GetContainerServicePowersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServicePowersResponse'
            Prelude.<$> (x Prelude..?> "powers" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetContainerServicePowers

instance Prelude.NFData GetContainerServicePowers

instance Prelude.ToHeaders GetContainerServicePowers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetContainerServicePowers" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetContainerServicePowers where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetContainerServicePowers where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetContainerServicePowers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerServicePowersResponse' smart constructor.
data GetContainerServicePowersResponse = GetContainerServicePowersResponse'
  { -- | An array of objects that describe the powers that can be specified for a
    -- container service.
    powers :: Prelude.Maybe [ContainerServicePower],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetContainerServicePowersResponse
newGetContainerServicePowersResponse pHttpStatus_ =
  GetContainerServicePowersResponse'
    { powers =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the powers that can be specified for a
-- container service.
getContainerServicePowersResponse_powers :: Lens.Lens' GetContainerServicePowersResponse (Prelude.Maybe [ContainerServicePower])
getContainerServicePowersResponse_powers = Lens.lens (\GetContainerServicePowersResponse' {powers} -> powers) (\s@GetContainerServicePowersResponse' {} a -> s {powers = a} :: GetContainerServicePowersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getContainerServicePowersResponse_httpStatus :: Lens.Lens' GetContainerServicePowersResponse Prelude.Int
getContainerServicePowersResponse_httpStatus = Lens.lens (\GetContainerServicePowersResponse' {httpStatus} -> httpStatus) (\s@GetContainerServicePowersResponse' {} a -> s {httpStatus = a} :: GetContainerServicePowersResponse)

instance
  Prelude.NFData
    GetContainerServicePowersResponse
