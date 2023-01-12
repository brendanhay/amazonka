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
-- Module      : Amazonka.DeviceFarm.GetNetworkProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a network profile.
module Amazonka.DeviceFarm.GetNetworkProfile
  ( -- * Creating a Request
    GetNetworkProfile (..),
    newGetNetworkProfile,

    -- * Request Lenses
    getNetworkProfile_arn,

    -- * Destructuring the Response
    GetNetworkProfileResponse (..),
    newGetNetworkProfileResponse,

    -- * Response Lenses
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkProfile' smart constructor.
data GetNetworkProfile = GetNetworkProfile'
  { -- | The ARN of the network profile to return information about.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getNetworkProfile_arn' - The ARN of the network profile to return information about.
newGetNetworkProfile ::
  -- | 'arn'
  Prelude.Text ->
  GetNetworkProfile
newGetNetworkProfile pArn_ =
  GetNetworkProfile' {arn = pArn_}

-- | The ARN of the network profile to return information about.
getNetworkProfile_arn :: Lens.Lens' GetNetworkProfile Prelude.Text
getNetworkProfile_arn = Lens.lens (\GetNetworkProfile' {arn} -> arn) (\s@GetNetworkProfile' {} a -> s {arn = a} :: GetNetworkProfile)

instance Core.AWSRequest GetNetworkProfile where
  type
    AWSResponse GetNetworkProfile =
      GetNetworkProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkProfileResponse'
            Prelude.<$> (x Data..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkProfile where
  hashWithSalt _salt GetNetworkProfile' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetNetworkProfile where
  rnf GetNetworkProfile' {..} = Prelude.rnf arn

instance Data.ToHeaders GetNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetNetworkProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNetworkProfile where
  toJSON GetNetworkProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetNetworkProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery GetNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { -- | The network profile.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkProfile', 'getNetworkProfileResponse_networkProfile' - The network profile.
--
-- 'httpStatus', 'getNetworkProfileResponse_httpStatus' - The response's http status code.
newGetNetworkProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkProfileResponse
newGetNetworkProfileResponse pHttpStatus_ =
  GetNetworkProfileResponse'
    { networkProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network profile.
getNetworkProfileResponse_networkProfile :: Lens.Lens' GetNetworkProfileResponse (Prelude.Maybe NetworkProfile)
getNetworkProfileResponse_networkProfile = Lens.lens (\GetNetworkProfileResponse' {networkProfile} -> networkProfile) (\s@GetNetworkProfileResponse' {} a -> s {networkProfile = a} :: GetNetworkProfileResponse)

-- | The response's http status code.
getNetworkProfileResponse_httpStatus :: Lens.Lens' GetNetworkProfileResponse Prelude.Int
getNetworkProfileResponse_httpStatus = Lens.lens (\GetNetworkProfileResponse' {httpStatus} -> httpStatus) (\s@GetNetworkProfileResponse' {} a -> s {httpStatus = a} :: GetNetworkProfileResponse)

instance Prelude.NFData GetNetworkProfileResponse where
  rnf GetNetworkProfileResponse' {..} =
    Prelude.rnf networkProfile
      `Prelude.seq` Prelude.rnf httpStatus
