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
-- Module      : Amazonka.RolesAnywhere.EnableProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the roles in a profile to receive session credentials in
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>.
--
-- __Required permissions:__ @rolesanywhere:EnableProfile@.
module Amazonka.RolesAnywhere.EnableProfile
  ( -- * Creating a Request
    EnableProfile (..),
    newEnableProfile,

    -- * Request Lenses
    enableProfile_profileId,

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

-- | /See:/ 'newEnableProfile' smart constructor.
data EnableProfile = EnableProfile'
  { -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'enableProfile_profileId' - The unique identifier of the profile.
newEnableProfile ::
  -- | 'profileId'
  Prelude.Text ->
  EnableProfile
newEnableProfile pProfileId_ =
  EnableProfile' {profileId = pProfileId_}

-- | The unique identifier of the profile.
enableProfile_profileId :: Lens.Lens' EnableProfile Prelude.Text
enableProfile_profileId = Lens.lens (\EnableProfile' {profileId} -> profileId) (\s@EnableProfile' {} a -> s {profileId = a} :: EnableProfile)

instance Core.AWSRequest EnableProfile where
  type
    AWSResponse EnableProfile =
      ProfileDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable EnableProfile where
  hashWithSalt _salt EnableProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData EnableProfile where
  rnf EnableProfile' {..} = Prelude.rnf profileId

instance Data.ToHeaders EnableProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableProfile where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath EnableProfile where
  toPath EnableProfile' {..} =
    Prelude.mconcat
      ["/profile/", Data.toBS profileId, "/enable"]

instance Data.ToQuery EnableProfile where
  toQuery = Prelude.const Prelude.mempty
