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
-- Module      : Network.AWS.SageMaker.Types.AppDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AppStatus
import Network.AWS.SageMaker.Types.AppType

-- | Details about an Amazon SageMaker app.
--
-- /See:/ 'newAppDetails' smart constructor.
data AppDetails = AppDetails'
  { -- | The status.
    status :: Core.Maybe AppStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The type of app.
    appType :: Core.Maybe AppType,
    -- | The name of the app.
    appName :: Core.Maybe Core.Text,
    -- | The user profile name.
    userProfileName :: Core.Maybe Core.Text,
    -- | The domain ID.
    domainId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AppDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'appDetails_status' - The status.
--
-- 'creationTime', 'appDetails_creationTime' - The creation time.
--
-- 'appType', 'appDetails_appType' - The type of app.
--
-- 'appName', 'appDetails_appName' - The name of the app.
--
-- 'userProfileName', 'appDetails_userProfileName' - The user profile name.
--
-- 'domainId', 'appDetails_domainId' - The domain ID.
newAppDetails ::
  AppDetails
newAppDetails =
  AppDetails'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      appType = Core.Nothing,
      appName = Core.Nothing,
      userProfileName = Core.Nothing,
      domainId = Core.Nothing
    }

-- | The status.
appDetails_status :: Lens.Lens' AppDetails (Core.Maybe AppStatus)
appDetails_status = Lens.lens (\AppDetails' {status} -> status) (\s@AppDetails' {} a -> s {status = a} :: AppDetails)

-- | The creation time.
appDetails_creationTime :: Lens.Lens' AppDetails (Core.Maybe Core.UTCTime)
appDetails_creationTime = Lens.lens (\AppDetails' {creationTime} -> creationTime) (\s@AppDetails' {} a -> s {creationTime = a} :: AppDetails) Core.. Lens.mapping Core._Time

-- | The type of app.
appDetails_appType :: Lens.Lens' AppDetails (Core.Maybe AppType)
appDetails_appType = Lens.lens (\AppDetails' {appType} -> appType) (\s@AppDetails' {} a -> s {appType = a} :: AppDetails)

-- | The name of the app.
appDetails_appName :: Lens.Lens' AppDetails (Core.Maybe Core.Text)
appDetails_appName = Lens.lens (\AppDetails' {appName} -> appName) (\s@AppDetails' {} a -> s {appName = a} :: AppDetails)

-- | The user profile name.
appDetails_userProfileName :: Lens.Lens' AppDetails (Core.Maybe Core.Text)
appDetails_userProfileName = Lens.lens (\AppDetails' {userProfileName} -> userProfileName) (\s@AppDetails' {} a -> s {userProfileName = a} :: AppDetails)

-- | The domain ID.
appDetails_domainId :: Lens.Lens' AppDetails (Core.Maybe Core.Text)
appDetails_domainId = Lens.lens (\AppDetails' {domainId} -> domainId) (\s@AppDetails' {} a -> s {domainId = a} :: AppDetails)

instance Core.FromJSON AppDetails where
  parseJSON =
    Core.withObject
      "AppDetails"
      ( \x ->
          AppDetails'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "AppType")
            Core.<*> (x Core..:? "AppName")
            Core.<*> (x Core..:? "UserProfileName")
            Core.<*> (x Core..:? "DomainId")
      )

instance Core.Hashable AppDetails

instance Core.NFData AppDetails
