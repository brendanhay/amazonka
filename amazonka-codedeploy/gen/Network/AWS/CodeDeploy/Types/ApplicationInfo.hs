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
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ApplicationInfo where

import Network.AWS.CodeDeploy.Types.ComputePlatform
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an application.
--
-- /See:/ 'newApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | True if the user has authenticated with GitHub for the specified
    -- application. Otherwise, false.
    linkedToGitHub :: Prelude.Maybe Prelude.Bool,
    -- | The name for a connection to a GitHub account.
    gitHubAccountName :: Prelude.Maybe Prelude.Text,
    -- | The time at which the application was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The destination platform type for deployment of the application
    -- (@Lambda@ or @Server@).
    computePlatform :: Prelude.Maybe ComputePlatform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApplicationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'applicationInfo_applicationId' - The application ID.
--
-- 'linkedToGitHub', 'applicationInfo_linkedToGitHub' - True if the user has authenticated with GitHub for the specified
-- application. Otherwise, false.
--
-- 'gitHubAccountName', 'applicationInfo_gitHubAccountName' - The name for a connection to a GitHub account.
--
-- 'createTime', 'applicationInfo_createTime' - The time at which the application was created.
--
-- 'applicationName', 'applicationInfo_applicationName' - The application name.
--
-- 'computePlatform', 'applicationInfo_computePlatform' - The destination platform type for deployment of the application
-- (@Lambda@ or @Server@).
newApplicationInfo ::
  ApplicationInfo
newApplicationInfo =
  ApplicationInfo'
    { applicationId = Prelude.Nothing,
      linkedToGitHub = Prelude.Nothing,
      gitHubAccountName = Prelude.Nothing,
      createTime = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      computePlatform = Prelude.Nothing
    }

-- | The application ID.
applicationInfo_applicationId :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_applicationId = Lens.lens (\ApplicationInfo' {applicationId} -> applicationId) (\s@ApplicationInfo' {} a -> s {applicationId = a} :: ApplicationInfo)

-- | True if the user has authenticated with GitHub for the specified
-- application. Otherwise, false.
applicationInfo_linkedToGitHub :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Bool)
applicationInfo_linkedToGitHub = Lens.lens (\ApplicationInfo' {linkedToGitHub} -> linkedToGitHub) (\s@ApplicationInfo' {} a -> s {linkedToGitHub = a} :: ApplicationInfo)

-- | The name for a connection to a GitHub account.
applicationInfo_gitHubAccountName :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_gitHubAccountName = Lens.lens (\ApplicationInfo' {gitHubAccountName} -> gitHubAccountName) (\s@ApplicationInfo' {} a -> s {gitHubAccountName = a} :: ApplicationInfo)

-- | The time at which the application was created.
applicationInfo_createTime :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.UTCTime)
applicationInfo_createTime = Lens.lens (\ApplicationInfo' {createTime} -> createTime) (\s@ApplicationInfo' {} a -> s {createTime = a} :: ApplicationInfo) Prelude.. Lens.mapping Prelude._Time

-- | The application name.
applicationInfo_applicationName :: Lens.Lens' ApplicationInfo (Prelude.Maybe Prelude.Text)
applicationInfo_applicationName = Lens.lens (\ApplicationInfo' {applicationName} -> applicationName) (\s@ApplicationInfo' {} a -> s {applicationName = a} :: ApplicationInfo)

-- | The destination platform type for deployment of the application
-- (@Lambda@ or @Server@).
applicationInfo_computePlatform :: Lens.Lens' ApplicationInfo (Prelude.Maybe ComputePlatform)
applicationInfo_computePlatform = Lens.lens (\ApplicationInfo' {computePlatform} -> computePlatform) (\s@ApplicationInfo' {} a -> s {computePlatform = a} :: ApplicationInfo)

instance Prelude.FromJSON ApplicationInfo where
  parseJSON =
    Prelude.withObject
      "ApplicationInfo"
      ( \x ->
          ApplicationInfo'
            Prelude.<$> (x Prelude..:? "applicationId")
            Prelude.<*> (x Prelude..:? "linkedToGitHub")
            Prelude.<*> (x Prelude..:? "gitHubAccountName")
            Prelude.<*> (x Prelude..:? "createTime")
            Prelude.<*> (x Prelude..:? "applicationName")
            Prelude.<*> (x Prelude..:? "computePlatform")
      )

instance Prelude.Hashable ApplicationInfo

instance Prelude.NFData ApplicationInfo
