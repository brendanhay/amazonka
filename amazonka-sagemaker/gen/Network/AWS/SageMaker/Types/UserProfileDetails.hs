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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.UserProfileStatus

-- | The user profile details.
--
-- /See:/ 'newUserProfileDetails' smart constructor.
data UserProfileDetails = UserProfileDetails'
  { -- | The status.
    status :: Core.Maybe UserProfileStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The user profile name.
    userProfileName :: Core.Maybe Core.Text,
    -- | The domain ID.
    domainId :: Core.Maybe Core.Text,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      userProfileName = Core.Nothing,
      domainId = Core.Nothing,
      lastModifiedTime = Core.Nothing
    }

-- | The status.
userProfileDetails_status :: Lens.Lens' UserProfileDetails (Core.Maybe UserProfileStatus)
userProfileDetails_status = Lens.lens (\UserProfileDetails' {status} -> status) (\s@UserProfileDetails' {} a -> s {status = a} :: UserProfileDetails)

-- | The creation time.
userProfileDetails_creationTime :: Lens.Lens' UserProfileDetails (Core.Maybe Core.UTCTime)
userProfileDetails_creationTime = Lens.lens (\UserProfileDetails' {creationTime} -> creationTime) (\s@UserProfileDetails' {} a -> s {creationTime = a} :: UserProfileDetails) Core.. Lens.mapping Core._Time

-- | The user profile name.
userProfileDetails_userProfileName :: Lens.Lens' UserProfileDetails (Core.Maybe Core.Text)
userProfileDetails_userProfileName = Lens.lens (\UserProfileDetails' {userProfileName} -> userProfileName) (\s@UserProfileDetails' {} a -> s {userProfileName = a} :: UserProfileDetails)

-- | The domain ID.
userProfileDetails_domainId :: Lens.Lens' UserProfileDetails (Core.Maybe Core.Text)
userProfileDetails_domainId = Lens.lens (\UserProfileDetails' {domainId} -> domainId) (\s@UserProfileDetails' {} a -> s {domainId = a} :: UserProfileDetails)

-- | The last modified time.
userProfileDetails_lastModifiedTime :: Lens.Lens' UserProfileDetails (Core.Maybe Core.UTCTime)
userProfileDetails_lastModifiedTime = Lens.lens (\UserProfileDetails' {lastModifiedTime} -> lastModifiedTime) (\s@UserProfileDetails' {} a -> s {lastModifiedTime = a} :: UserProfileDetails) Core.. Lens.mapping Core._Time

instance Core.FromJSON UserProfileDetails where
  parseJSON =
    Core.withObject
      "UserProfileDetails"
      ( \x ->
          UserProfileDetails'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "UserProfileName")
            Core.<*> (x Core..:? "DomainId")
            Core.<*> (x Core..:? "LastModifiedTime")
      )

instance Core.Hashable UserProfileDetails

instance Core.NFData UserProfileDetails
