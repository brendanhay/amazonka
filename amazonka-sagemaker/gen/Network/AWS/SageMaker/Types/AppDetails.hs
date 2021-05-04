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
-- Module      : Network.AWS.SageMaker.Types.AppDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AppStatus
import Network.AWS.SageMaker.Types.AppType

-- | Details about an Amazon SageMaker app.
--
-- /See:/ 'newAppDetails' smart constructor.
data AppDetails = AppDetails'
  { -- | The status.
    status :: Prelude.Maybe AppStatus,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The type of app.
    appType :: Prelude.Maybe AppType,
    -- | The name of the app.
    appName :: Prelude.Maybe Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      appType = Prelude.Nothing,
      appName = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = Prelude.Nothing
    }

-- | The status.
appDetails_status :: Lens.Lens' AppDetails (Prelude.Maybe AppStatus)
appDetails_status = Lens.lens (\AppDetails' {status} -> status) (\s@AppDetails' {} a -> s {status = a} :: AppDetails)

-- | The creation time.
appDetails_creationTime :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.UTCTime)
appDetails_creationTime = Lens.lens (\AppDetails' {creationTime} -> creationTime) (\s@AppDetails' {} a -> s {creationTime = a} :: AppDetails) Prelude.. Lens.mapping Prelude._Time

-- | The type of app.
appDetails_appType :: Lens.Lens' AppDetails (Prelude.Maybe AppType)
appDetails_appType = Lens.lens (\AppDetails' {appType} -> appType) (\s@AppDetails' {} a -> s {appType = a} :: AppDetails)

-- | The name of the app.
appDetails_appName :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_appName = Lens.lens (\AppDetails' {appName} -> appName) (\s@AppDetails' {} a -> s {appName = a} :: AppDetails)

-- | The user profile name.
appDetails_userProfileName :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_userProfileName = Lens.lens (\AppDetails' {userProfileName} -> userProfileName) (\s@AppDetails' {} a -> s {userProfileName = a} :: AppDetails)

-- | The domain ID.
appDetails_domainId :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_domainId = Lens.lens (\AppDetails' {domainId} -> domainId) (\s@AppDetails' {} a -> s {domainId = a} :: AppDetails)

instance Prelude.FromJSON AppDetails where
  parseJSON =
    Prelude.withObject
      "AppDetails"
      ( \x ->
          AppDetails'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "AppType")
            Prelude.<*> (x Prelude..:? "AppName")
            Prelude.<*> (x Prelude..:? "UserProfileName")
            Prelude.<*> (x Prelude..:? "DomainId")
      )

instance Prelude.Hashable AppDetails

instance Prelude.NFData AppDetails
