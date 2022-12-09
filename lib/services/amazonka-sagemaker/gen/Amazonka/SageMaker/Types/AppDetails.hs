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
-- Module      : Amazonka.SageMaker.Types.AppDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AppDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AppStatus
import Amazonka.SageMaker.Types.AppType

-- | Details about an Amazon SageMaker app.
--
-- /See:/ 'newAppDetails' smart constructor.
data AppDetails = AppDetails'
  { -- | The name of the app.
    appName :: Prelude.Maybe Prelude.Text,
    -- | The type of app.
    appType :: Prelude.Maybe AppType,
    -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The name of the space.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe AppStatus,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appName', 'appDetails_appName' - The name of the app.
--
-- 'appType', 'appDetails_appType' - The type of app.
--
-- 'creationTime', 'appDetails_creationTime' - The creation time.
--
-- 'domainId', 'appDetails_domainId' - The domain ID.
--
-- 'spaceName', 'appDetails_spaceName' - The name of the space.
--
-- 'status', 'appDetails_status' - The status.
--
-- 'userProfileName', 'appDetails_userProfileName' - The user profile name.
newAppDetails ::
  AppDetails
newAppDetails =
  AppDetails'
    { appName = Prelude.Nothing,
      appType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing,
      spaceName = Prelude.Nothing,
      status = Prelude.Nothing,
      userProfileName = Prelude.Nothing
    }

-- | The name of the app.
appDetails_appName :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_appName = Lens.lens (\AppDetails' {appName} -> appName) (\s@AppDetails' {} a -> s {appName = a} :: AppDetails)

-- | The type of app.
appDetails_appType :: Lens.Lens' AppDetails (Prelude.Maybe AppType)
appDetails_appType = Lens.lens (\AppDetails' {appType} -> appType) (\s@AppDetails' {} a -> s {appType = a} :: AppDetails)

-- | The creation time.
appDetails_creationTime :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.UTCTime)
appDetails_creationTime = Lens.lens (\AppDetails' {creationTime} -> creationTime) (\s@AppDetails' {} a -> s {creationTime = a} :: AppDetails) Prelude.. Lens.mapping Data._Time

-- | The domain ID.
appDetails_domainId :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_domainId = Lens.lens (\AppDetails' {domainId} -> domainId) (\s@AppDetails' {} a -> s {domainId = a} :: AppDetails)

-- | The name of the space.
appDetails_spaceName :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_spaceName = Lens.lens (\AppDetails' {spaceName} -> spaceName) (\s@AppDetails' {} a -> s {spaceName = a} :: AppDetails)

-- | The status.
appDetails_status :: Lens.Lens' AppDetails (Prelude.Maybe AppStatus)
appDetails_status = Lens.lens (\AppDetails' {status} -> status) (\s@AppDetails' {} a -> s {status = a} :: AppDetails)

-- | The user profile name.
appDetails_userProfileName :: Lens.Lens' AppDetails (Prelude.Maybe Prelude.Text)
appDetails_userProfileName = Lens.lens (\AppDetails' {userProfileName} -> userProfileName) (\s@AppDetails' {} a -> s {userProfileName = a} :: AppDetails)

instance Data.FromJSON AppDetails where
  parseJSON =
    Data.withObject
      "AppDetails"
      ( \x ->
          AppDetails'
            Prelude.<$> (x Data..:? "AppName")
            Prelude.<*> (x Data..:? "AppType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "SpaceName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserProfileName")
      )

instance Prelude.Hashable AppDetails where
  hashWithSalt _salt AppDetails' {..} =
    _salt `Prelude.hashWithSalt` appName
      `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` spaceName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData AppDetails where
  rnf AppDetails' {..} =
    Prelude.rnf appName
      `Prelude.seq` Prelude.rnf appType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf spaceName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf userProfileName
