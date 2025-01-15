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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.UserProfileDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
-- /See:/ 'newUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'userProfileDetails_creationTime' - The creation time.
--
-- 'domainId', 'userProfileDetails_domainId' - The domain ID.
--
-- 'lastModifiedTime', 'userProfileDetails_lastModifiedTime' - The last modified time.
--
-- 'status', 'userProfileDetails_status' - The status.
--
-- 'userProfileName', 'userProfileDetails_userProfileName' - The user profile name.
newUserProfileDetails ::
  UserProfileDetails
newUserProfileDetails =
  UserProfileDetails'
    { creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      status = Prelude.Nothing,
      userProfileName = Prelude.Nothing
    }

-- | The creation time.
userProfileDetails_creationTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_creationTime = Lens.lens (\UserProfileDetails' {creationTime} -> creationTime) (\s@UserProfileDetails' {} a -> s {creationTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Data._Time

-- | The domain ID.
userProfileDetails_domainId :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_domainId = Lens.lens (\UserProfileDetails' {domainId} -> domainId) (\s@UserProfileDetails' {} a -> s {domainId = a} :: UserProfileDetails)

-- | The last modified time.
userProfileDetails_lastModifiedTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_lastModifiedTime = Lens.lens (\UserProfileDetails' {lastModifiedTime} -> lastModifiedTime) (\s@UserProfileDetails' {} a -> s {lastModifiedTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Data._Time

-- | The status.
userProfileDetails_status :: Lens.Lens' UserProfileDetails (Prelude.Maybe UserProfileStatus)
userProfileDetails_status = Lens.lens (\UserProfileDetails' {status} -> status) (\s@UserProfileDetails' {} a -> s {status = a} :: UserProfileDetails)

-- | The user profile name.
userProfileDetails_userProfileName :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_userProfileName = Lens.lens (\UserProfileDetails' {userProfileName} -> userProfileName) (\s@UserProfileDetails' {} a -> s {userProfileName = a} :: UserProfileDetails)

instance Data.FromJSON UserProfileDetails where
  parseJSON =
    Data.withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserProfileName")
      )

instance Prelude.Hashable UserProfileDetails where
  hashWithSalt _salt UserProfileDetails' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData UserProfileDetails where
  rnf UserProfileDetails' {..} =
    Prelude.rnf creationTime `Prelude.seq`
      Prelude.rnf domainId `Prelude.seq`
        Prelude.rnf lastModifiedTime `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf userProfileName
