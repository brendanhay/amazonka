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
-- Module      : Amazonka.RolesAnywhere.GetProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a profile.
--
-- __Required permissions:__ @rolesanywhere:GetProfile@.
module Amazonka.RolesAnywhere.GetProfile
  ( -- * Creating a Request
    GetProfile (..),
    newGetProfile,

    -- * Request Lenses
    getProfile_profileId,

    -- * Destructuring the Response
    ProfileDetailResponse (..),
    newProfileDetailResponse,

    -- * Response Lenses
    profileDetailResponse_profile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newGetProfile' smart constructor.
data GetProfile = GetProfile'
  { -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'getProfile_profileId' - The unique identifier of the profile.
newGetProfile ::
  -- | 'profileId'
  Prelude.Text ->
  GetProfile
newGetProfile pProfileId_ =
  GetProfile' {profileId = pProfileId_}

-- | The unique identifier of the profile.
getProfile_profileId :: Lens.Lens' GetProfile Prelude.Text
getProfile_profileId = Lens.lens (\GetProfile' {profileId} -> profileId) (\s@GetProfile' {} a -> s {profileId = a} :: GetProfile)

instance Core.AWSRequest GetProfile where
  type AWSResponse GetProfile = ProfileDetailResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetProfile where
  hashWithSalt _salt GetProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData GetProfile where
  rnf GetProfile' {..} = Prelude.rnf profileId

instance Data.ToHeaders GetProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetProfile where
  toPath GetProfile' {..} =
    Prelude.mconcat ["/profile/", Data.toBS profileId]

instance Data.ToQuery GetProfile where
  toQuery = Prelude.const Prelude.mempty
