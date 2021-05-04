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
-- Module      : Network.AWS.DeviceFarm.GetNetworkProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a network profile.
module Network.AWS.DeviceFarm.GetNetworkProfile
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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetNetworkProfile' smart constructor.
data GetNetworkProfile = GetNetworkProfile'
  { -- | The ARN of the network profile to return information about.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetNetworkProfile where
  type Rs GetNetworkProfile = GetNetworkProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkProfileResponse'
            Prelude.<$> (x Prelude..?> "networkProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkProfile

instance Prelude.NFData GetNetworkProfile

instance Prelude.ToHeaders GetNetworkProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.GetNetworkProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetNetworkProfile where
  toJSON GetNetworkProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath GetNetworkProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetNetworkProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkProfileResponse' smart constructor.
data GetNetworkProfileResponse = GetNetworkProfileResponse'
  { -- | The network profile.
    networkProfile :: Prelude.Maybe NetworkProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetNetworkProfileResponse
