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
-- Module      : Amazonka.Transfer.DescribeProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of the profile that\'s specified by the @ProfileId@.
module Amazonka.Transfer.DescribeProfile
  ( -- * Creating a Request
    DescribeProfile (..),
    newDescribeProfile,

    -- * Request Lenses
    describeProfile_profileId,

    -- * Destructuring the Response
    DescribeProfileResponse (..),
    newDescribeProfileResponse,

    -- * Response Lenses
    describeProfileResponse_httpStatus,
    describeProfileResponse_profile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeProfile' smart constructor.
data DescribeProfile = DescribeProfile'
  { -- | The identifier of the profile that you want described.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'describeProfile_profileId' - The identifier of the profile that you want described.
newDescribeProfile ::
  -- | 'profileId'
  Prelude.Text ->
  DescribeProfile
newDescribeProfile pProfileId_ =
  DescribeProfile' {profileId = pProfileId_}

-- | The identifier of the profile that you want described.
describeProfile_profileId :: Lens.Lens' DescribeProfile Prelude.Text
describeProfile_profileId = Lens.lens (\DescribeProfile' {profileId} -> profileId) (\s@DescribeProfile' {} a -> s {profileId = a} :: DescribeProfile)

instance Core.AWSRequest DescribeProfile where
  type
    AWSResponse DescribeProfile =
      DescribeProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Profile")
      )

instance Prelude.Hashable DescribeProfile where
  hashWithSalt _salt DescribeProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData DescribeProfile where
  rnf DescribeProfile' {..} = Prelude.rnf profileId

instance Core.ToHeaders DescribeProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DescribeProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProfile where
  toJSON DescribeProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProfileId" Core..= profileId)]
      )

instance Core.ToPath DescribeProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProfileResponse' smart constructor.
data DescribeProfileResponse = DescribeProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The details of the specified profile, returned as an object.
    profile :: DescribedProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeProfileResponse_httpStatus' - The response's http status code.
--
-- 'profile', 'describeProfileResponse_profile' - The details of the specified profile, returned as an object.
newDescribeProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profile'
  DescribedProfile ->
  DescribeProfileResponse
newDescribeProfileResponse pHttpStatus_ pProfile_ =
  DescribeProfileResponse'
    { httpStatus = pHttpStatus_,
      profile = pProfile_
    }

-- | The response's http status code.
describeProfileResponse_httpStatus :: Lens.Lens' DescribeProfileResponse Prelude.Int
describeProfileResponse_httpStatus = Lens.lens (\DescribeProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeProfileResponse' {} a -> s {httpStatus = a} :: DescribeProfileResponse)

-- | The details of the specified profile, returned as an object.
describeProfileResponse_profile :: Lens.Lens' DescribeProfileResponse DescribedProfile
describeProfileResponse_profile = Lens.lens (\DescribeProfileResponse' {profile} -> profile) (\s@DescribeProfileResponse' {} a -> s {profile = a} :: DescribeProfileResponse)

instance Prelude.NFData DescribeProfileResponse where
  rnf DescribeProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profile
