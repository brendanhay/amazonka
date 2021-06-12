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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an application.
--
-- /See:/ 'newApplicationInfo' smart constructor.
data ApplicationInfo = ApplicationInfo'
  { -- | The application ID.
    applicationId :: Core.Maybe Core.Text,
    -- | True if the user has authenticated with GitHub for the specified
    -- application. Otherwise, false.
    linkedToGitHub :: Core.Maybe Core.Bool,
    -- | The name for a connection to a GitHub account.
    gitHubAccountName :: Core.Maybe Core.Text,
    -- | The time at which the application was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | The application name.
    applicationName :: Core.Maybe Core.Text,
    -- | The destination platform type for deployment of the application
    -- (@Lambda@ or @Server@).
    computePlatform :: Core.Maybe ComputePlatform
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { applicationId = Core.Nothing,
      linkedToGitHub = Core.Nothing,
      gitHubAccountName = Core.Nothing,
      createTime = Core.Nothing,
      applicationName = Core.Nothing,
      computePlatform = Core.Nothing
    }

-- | The application ID.
applicationInfo_applicationId :: Lens.Lens' ApplicationInfo (Core.Maybe Core.Text)
applicationInfo_applicationId = Lens.lens (\ApplicationInfo' {applicationId} -> applicationId) (\s@ApplicationInfo' {} a -> s {applicationId = a} :: ApplicationInfo)

-- | True if the user has authenticated with GitHub for the specified
-- application. Otherwise, false.
applicationInfo_linkedToGitHub :: Lens.Lens' ApplicationInfo (Core.Maybe Core.Bool)
applicationInfo_linkedToGitHub = Lens.lens (\ApplicationInfo' {linkedToGitHub} -> linkedToGitHub) (\s@ApplicationInfo' {} a -> s {linkedToGitHub = a} :: ApplicationInfo)

-- | The name for a connection to a GitHub account.
applicationInfo_gitHubAccountName :: Lens.Lens' ApplicationInfo (Core.Maybe Core.Text)
applicationInfo_gitHubAccountName = Lens.lens (\ApplicationInfo' {gitHubAccountName} -> gitHubAccountName) (\s@ApplicationInfo' {} a -> s {gitHubAccountName = a} :: ApplicationInfo)

-- | The time at which the application was created.
applicationInfo_createTime :: Lens.Lens' ApplicationInfo (Core.Maybe Core.UTCTime)
applicationInfo_createTime = Lens.lens (\ApplicationInfo' {createTime} -> createTime) (\s@ApplicationInfo' {} a -> s {createTime = a} :: ApplicationInfo) Core.. Lens.mapping Core._Time

-- | The application name.
applicationInfo_applicationName :: Lens.Lens' ApplicationInfo (Core.Maybe Core.Text)
applicationInfo_applicationName = Lens.lens (\ApplicationInfo' {applicationName} -> applicationName) (\s@ApplicationInfo' {} a -> s {applicationName = a} :: ApplicationInfo)

-- | The destination platform type for deployment of the application
-- (@Lambda@ or @Server@).
applicationInfo_computePlatform :: Lens.Lens' ApplicationInfo (Core.Maybe ComputePlatform)
applicationInfo_computePlatform = Lens.lens (\ApplicationInfo' {computePlatform} -> computePlatform) (\s@ApplicationInfo' {} a -> s {computePlatform = a} :: ApplicationInfo)

instance Core.FromJSON ApplicationInfo where
  parseJSON =
    Core.withObject
      "ApplicationInfo"
      ( \x ->
          ApplicationInfo'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "linkedToGitHub")
            Core.<*> (x Core..:? "gitHubAccountName")
            Core.<*> (x Core..:? "createTime")
            Core.<*> (x Core..:? "applicationName")
            Core.<*> (x Core..:? "computePlatform")
      )

instance Core.Hashable ApplicationInfo

instance Core.NFData ApplicationInfo
