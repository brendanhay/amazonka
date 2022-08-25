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
-- Module      : Amazonka.Transfer.Types.ListedProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.ProfileType

-- | Returns the properties of the profile that was specified.
--
-- /See:/ 'newListedProfile' smart constructor.
data ListedProfile = ListedProfile'
  { -- | A unique identifier for the local or partner AS2 profile.
    profileId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the AS2 process.
    as2Id :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the specified profile.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
    -- type profiles. If not supplied in the request, the command lists all
    -- types of profiles.
    profileType :: Prelude.Maybe ProfileType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'listedProfile_profileId' - A unique identifier for the local or partner AS2 profile.
--
-- 'as2Id', 'listedProfile_as2Id' - The unique identifier for the AS2 process.
--
-- 'arn', 'listedProfile_arn' - The Amazon Resource Name (ARN) of the specified profile.
--
-- 'profileType', 'listedProfile_profileType' - Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
-- type profiles. If not supplied in the request, the command lists all
-- types of profiles.
newListedProfile ::
  ListedProfile
newListedProfile =
  ListedProfile'
    { profileId = Prelude.Nothing,
      as2Id = Prelude.Nothing,
      arn = Prelude.Nothing,
      profileType = Prelude.Nothing
    }

-- | A unique identifier for the local or partner AS2 profile.
listedProfile_profileId :: Lens.Lens' ListedProfile (Prelude.Maybe Prelude.Text)
listedProfile_profileId = Lens.lens (\ListedProfile' {profileId} -> profileId) (\s@ListedProfile' {} a -> s {profileId = a} :: ListedProfile)

-- | The unique identifier for the AS2 process.
listedProfile_as2Id :: Lens.Lens' ListedProfile (Prelude.Maybe Prelude.Text)
listedProfile_as2Id = Lens.lens (\ListedProfile' {as2Id} -> as2Id) (\s@ListedProfile' {} a -> s {as2Id = a} :: ListedProfile)

-- | The Amazon Resource Name (ARN) of the specified profile.
listedProfile_arn :: Lens.Lens' ListedProfile (Prelude.Maybe Prelude.Text)
listedProfile_arn = Lens.lens (\ListedProfile' {arn} -> arn) (\s@ListedProfile' {} a -> s {arn = a} :: ListedProfile)

-- | Indicates whether to list only @LOCAL@ type profiles or only @PARTNER@
-- type profiles. If not supplied in the request, the command lists all
-- types of profiles.
listedProfile_profileType :: Lens.Lens' ListedProfile (Prelude.Maybe ProfileType)
listedProfile_profileType = Lens.lens (\ListedProfile' {profileType} -> profileType) (\s@ListedProfile' {} a -> s {profileType = a} :: ListedProfile)

instance Core.FromJSON ListedProfile where
  parseJSON =
    Core.withObject
      "ListedProfile"
      ( \x ->
          ListedProfile'
            Prelude.<$> (x Core..:? "ProfileId")
            Prelude.<*> (x Core..:? "As2Id")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "ProfileType")
      )

instance Prelude.Hashable ListedProfile where
  hashWithSalt _salt ListedProfile' {..} =
    _salt `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` as2Id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` profileType

instance Prelude.NFData ListedProfile where
  rnf ListedProfile' {..} =
    Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf as2Id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf profileType
