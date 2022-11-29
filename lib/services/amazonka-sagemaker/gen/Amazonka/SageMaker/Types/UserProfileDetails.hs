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
-- Module      : Amazonka.SageMaker.Types.UserProfileDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserProfileDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
-- /See:/ 'newUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The creation time.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserProfileDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'userProfileDetails_status' - The status.
--
-- 'lastModifiedTime', 'userProfileDetails_lastModifiedTime' - The last modified time.
--
-- 'userProfileName', 'userProfileDetails_userProfileName' - The user profile name.
--
-- 'creationTime', 'userProfileDetails_creationTime' - The creation time.
--
-- 'domainId', 'userProfileDetails_domainId' - The domain ID.
newUserProfileDetails ::
  UserProfileDetails
newUserProfileDetails =
  UserProfileDetails'
    { status = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing
    }

-- | The status.
userProfileDetails_status :: Lens.Lens' UserProfileDetails (Prelude.Maybe UserProfileStatus)
userProfileDetails_status = Lens.lens (\UserProfileDetails' {status} -> status) (\s@UserProfileDetails' {} a -> s {status = a} :: UserProfileDetails)

-- | The last modified time.
userProfileDetails_lastModifiedTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_lastModifiedTime = Lens.lens (\UserProfileDetails' {lastModifiedTime} -> lastModifiedTime) (\s@UserProfileDetails' {} a -> s {lastModifiedTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Core._Time

-- | The user profile name.
userProfileDetails_userProfileName :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_userProfileName = Lens.lens (\UserProfileDetails' {userProfileName} -> userProfileName) (\s@UserProfileDetails' {} a -> s {userProfileName = a} :: UserProfileDetails)

-- | The creation time.
userProfileDetails_creationTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_creationTime = Lens.lens (\UserProfileDetails' {creationTime} -> creationTime) (\s@UserProfileDetails' {} a -> s {creationTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Core._Time

-- | The domain ID.
userProfileDetails_domainId :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_domainId = Lens.lens (\UserProfileDetails' {domainId} -> domainId) (\s@UserProfileDetails' {} a -> s {domainId = a} :: UserProfileDetails)

instance Core.FromJSON UserProfileDetails where
  parseJSON =
    Core.withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "UserProfileName")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "DomainId")
      )

instance Prelude.Hashable UserProfileDetails where
  hashWithSalt _salt UserProfileDetails' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` userProfileName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData UserProfileDetails where
  rnf UserProfileDetails' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf userProfileName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf domainId
