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
-- Module      : Amazonka.RolesAnywhere.DisableProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables a profile. When disabled, temporary credential requests with
-- this profile fail.
--
-- __Required permissions:__ @rolesanywhere:DisableProfile@.
module Amazonka.RolesAnywhere.DisableProfile
  ( -- * Creating a Request
    DisableProfile (..),
    newDisableProfile,

    -- * Request Lenses
    disableProfile_profileId,

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

-- | /See:/ 'newDisableProfile' smart constructor.
data DisableProfile = DisableProfile'
  { -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'disableProfile_profileId' - The unique identifier of the profile.
newDisableProfile ::
  -- | 'profileId'
  Prelude.Text ->
  DisableProfile
newDisableProfile pProfileId_ =
  DisableProfile' {profileId = pProfileId_}

-- | The unique identifier of the profile.
disableProfile_profileId :: Lens.Lens' DisableProfile Prelude.Text
disableProfile_profileId = Lens.lens (\DisableProfile' {profileId} -> profileId) (\s@DisableProfile' {} a -> s {profileId = a} :: DisableProfile)

instance Core.AWSRequest DisableProfile where
  type
    AWSResponse DisableProfile =
      ProfileDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DisableProfile where
  hashWithSalt _salt DisableProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData DisableProfile where
  rnf DisableProfile' {..} = Prelude.rnf profileId

instance Data.ToHeaders DisableProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableProfile where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisableProfile where
  toPath DisableProfile' {..} =
    Prelude.mconcat
      ["/profile/", Data.toBS profileId, "/disable"]

instance Data.ToQuery DisableProfile where
  toQuery = Prelude.const Prelude.mempty
