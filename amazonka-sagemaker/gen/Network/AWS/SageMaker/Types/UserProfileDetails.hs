{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.UserProfileDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
-- /See:/ 'newUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'creationTime', 'userProfileDetails_creationTime' - The creation time.
--
-- 'userProfileName', 'userProfileDetails_userProfileName' - The user profile name.
--
-- 'domainId', 'userProfileDetails_domainId' - The domain ID.
--
-- 'lastModifiedTime', 'userProfileDetails_lastModifiedTime' - The last modified time.
newUserProfileDetails ::
  UserProfileDetails
newUserProfileDetails =
  UserProfileDetails'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing
    }

-- | The status.
userProfileDetails_status :: Lens.Lens' UserProfileDetails (Prelude.Maybe UserProfileStatus)
userProfileDetails_status = Lens.lens (\UserProfileDetails' {status} -> status) (\s@UserProfileDetails' {} a -> s {status = a} :: UserProfileDetails)

-- | The creation time.
userProfileDetails_creationTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_creationTime = Lens.lens (\UserProfileDetails' {creationTime} -> creationTime) (\s@UserProfileDetails' {} a -> s {creationTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Prelude._Time

-- | The user profile name.
userProfileDetails_userProfileName :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_userProfileName = Lens.lens (\UserProfileDetails' {userProfileName} -> userProfileName) (\s@UserProfileDetails' {} a -> s {userProfileName = a} :: UserProfileDetails)

-- | The domain ID.
userProfileDetails_domainId :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.Text)
userProfileDetails_domainId = Lens.lens (\UserProfileDetails' {domainId} -> domainId) (\s@UserProfileDetails' {} a -> s {domainId = a} :: UserProfileDetails)

-- | The last modified time.
userProfileDetails_lastModifiedTime :: Lens.Lens' UserProfileDetails (Prelude.Maybe Prelude.UTCTime)
userProfileDetails_lastModifiedTime = Lens.lens (\UserProfileDetails' {lastModifiedTime} -> lastModifiedTime) (\s@UserProfileDetails' {} a -> s {lastModifiedTime = a} :: UserProfileDetails) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON UserProfileDetails where
  parseJSON =
    Prelude.withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "UserProfileName")
            Prelude.<*> (x Prelude..:? "DomainId")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
      )

instance Prelude.Hashable UserProfileDetails

instance Prelude.NFData UserProfileDetails
