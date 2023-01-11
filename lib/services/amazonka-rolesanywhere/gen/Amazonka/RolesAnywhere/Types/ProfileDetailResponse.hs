{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.Types.ProfileDetailResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.ProfileDetailResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RolesAnywhere.Types.ProfileDetail

-- | /See:/ 'newProfileDetailResponse' smart constructor.
data ProfileDetailResponse = ProfileDetailResponse'
  { -- | The state of the profile after a read or write operation.
    profile :: Prelude.Maybe ProfileDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profile', 'profileDetailResponse_profile' - The state of the profile after a read or write operation.
newProfileDetailResponse ::
  ProfileDetailResponse
newProfileDetailResponse =
  ProfileDetailResponse' {profile = Prelude.Nothing}

-- | The state of the profile after a read or write operation.
profileDetailResponse_profile :: Lens.Lens' ProfileDetailResponse (Prelude.Maybe ProfileDetail)
profileDetailResponse_profile = Lens.lens (\ProfileDetailResponse' {profile} -> profile) (\s@ProfileDetailResponse' {} a -> s {profile = a} :: ProfileDetailResponse)

instance Data.FromJSON ProfileDetailResponse where
  parseJSON =
    Data.withObject
      "ProfileDetailResponse"
      ( \x ->
          ProfileDetailResponse'
            Prelude.<$> (x Data..:? "profile")
      )

instance Prelude.Hashable ProfileDetailResponse where
  hashWithSalt _salt ProfileDetailResponse' {..} =
    _salt `Prelude.hashWithSalt` profile

instance Prelude.NFData ProfileDetailResponse where
  rnf ProfileDetailResponse' {..} = Prelude.rnf profile
