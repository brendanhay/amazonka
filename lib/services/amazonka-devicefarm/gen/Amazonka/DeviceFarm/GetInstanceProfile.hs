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
-- Module      : Amazonka.DeviceFarm.GetInstanceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified instance profile.
module Amazonka.DeviceFarm.GetInstanceProfile
  ( -- * Creating a Request
    GetInstanceProfile (..),
    newGetInstanceProfile,

    -- * Request Lenses
    getInstanceProfile_arn,

    -- * Destructuring the Response
    GetInstanceProfileResponse (..),
    newGetInstanceProfileResponse,

    -- * Response Lenses
    getInstanceProfileResponse_instanceProfile,
    getInstanceProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceProfile' smart constructor.
data GetInstanceProfile = GetInstanceProfile'
  { -- | The Amazon Resource Name (ARN) of an instance profile.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getInstanceProfile_arn' - The Amazon Resource Name (ARN) of an instance profile.
newGetInstanceProfile ::
  -- | 'arn'
  Prelude.Text ->
  GetInstanceProfile
newGetInstanceProfile pArn_ =
  GetInstanceProfile' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of an instance profile.
getInstanceProfile_arn :: Lens.Lens' GetInstanceProfile Prelude.Text
getInstanceProfile_arn = Lens.lens (\GetInstanceProfile' {arn} -> arn) (\s@GetInstanceProfile' {} a -> s {arn = a} :: GetInstanceProfile)

instance Core.AWSRequest GetInstanceProfile where
  type
    AWSResponse GetInstanceProfile =
      GetInstanceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceProfileResponse'
            Prelude.<$> (x Data..?> "instanceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceProfile where
  hashWithSalt _salt GetInstanceProfile' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetInstanceProfile where
  rnf GetInstanceProfile' {..} = Prelude.rnf arn

instance Data.ToHeaders GetInstanceProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetInstanceProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstanceProfile where
  toJSON GetInstanceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetInstanceProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstanceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceProfileResponse' smart constructor.
data GetInstanceProfileResponse = GetInstanceProfileResponse'
  { -- | An object that contains information about an instance profile.
    instanceProfile :: Prelude.Maybe InstanceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceProfile', 'getInstanceProfileResponse_instanceProfile' - An object that contains information about an instance profile.
--
-- 'httpStatus', 'getInstanceProfileResponse_httpStatus' - The response's http status code.
newGetInstanceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceProfileResponse
newGetInstanceProfileResponse pHttpStatus_ =
  GetInstanceProfileResponse'
    { instanceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about an instance profile.
getInstanceProfileResponse_instanceProfile :: Lens.Lens' GetInstanceProfileResponse (Prelude.Maybe InstanceProfile)
getInstanceProfileResponse_instanceProfile = Lens.lens (\GetInstanceProfileResponse' {instanceProfile} -> instanceProfile) (\s@GetInstanceProfileResponse' {} a -> s {instanceProfile = a} :: GetInstanceProfileResponse)

-- | The response's http status code.
getInstanceProfileResponse_httpStatus :: Lens.Lens' GetInstanceProfileResponse Prelude.Int
getInstanceProfileResponse_httpStatus = Lens.lens (\GetInstanceProfileResponse' {httpStatus} -> httpStatus) (\s@GetInstanceProfileResponse' {} a -> s {httpStatus = a} :: GetInstanceProfileResponse)

instance Prelude.NFData GetInstanceProfileResponse where
  rnf GetInstanceProfileResponse' {..} =
    Prelude.rnf instanceProfile
      `Prelude.seq` Prelude.rnf httpStatus
