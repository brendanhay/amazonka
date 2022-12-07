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
-- Module      : Amazonka.RolesAnywhere.DeleteProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a profile.
--
-- __Required permissions:__ @rolesanywhere:DeleteProfile@.
module Amazonka.RolesAnywhere.DeleteProfile
  ( -- * Creating a Request
    DeleteProfile (..),
    newDeleteProfile,

    -- * Request Lenses
    deleteProfile_profileId,

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

-- | /See:/ 'newDeleteProfile' smart constructor.
data DeleteProfile = DeleteProfile'
  { -- | The unique identifier of the profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'deleteProfile_profileId' - The unique identifier of the profile.
newDeleteProfile ::
  -- | 'profileId'
  Prelude.Text ->
  DeleteProfile
newDeleteProfile pProfileId_ =
  DeleteProfile' {profileId = pProfileId_}

-- | The unique identifier of the profile.
deleteProfile_profileId :: Lens.Lens' DeleteProfile Prelude.Text
deleteProfile_profileId = Lens.lens (\DeleteProfile' {profileId} -> profileId) (\s@DeleteProfile' {} a -> s {profileId = a} :: DeleteProfile)

instance Core.AWSRequest DeleteProfile where
  type
    AWSResponse DeleteProfile =
      ProfileDetailResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteProfile where
  hashWithSalt _salt DeleteProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId

instance Prelude.NFData DeleteProfile where
  rnf DeleteProfile' {..} = Prelude.rnf profileId

instance Data.ToHeaders DeleteProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProfile where
  toPath DeleteProfile' {..} =
    Prelude.mconcat ["/profile/", Data.toBS profileId]

instance Data.ToQuery DeleteProfile where
  toQuery = Prelude.const Prelude.mempty
