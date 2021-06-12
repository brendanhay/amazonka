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
-- Module      : Network.AWS.CodeDeploy.Types.GenericRevisionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GenericRevisionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an application revision.
--
-- /See:/ 'newGenericRevisionInfo' smart constructor.
data GenericRevisionInfo = GenericRevisionInfo'
  { -- | When the revision was registered with AWS CodeDeploy.
    registerTime :: Core.Maybe Core.POSIX,
    -- | The deployment groups for which this is the current target revision.
    deploymentGroups :: Core.Maybe [Core.Text],
    -- | A comment about the revision.
    description :: Core.Maybe Core.Text,
    -- | When the revision was first used by AWS CodeDeploy.
    firstUsedTime :: Core.Maybe Core.POSIX,
    -- | When the revision was last used by AWS CodeDeploy.
    lastUsedTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenericRevisionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registerTime', 'genericRevisionInfo_registerTime' - When the revision was registered with AWS CodeDeploy.
--
-- 'deploymentGroups', 'genericRevisionInfo_deploymentGroups' - The deployment groups for which this is the current target revision.
--
-- 'description', 'genericRevisionInfo_description' - A comment about the revision.
--
-- 'firstUsedTime', 'genericRevisionInfo_firstUsedTime' - When the revision was first used by AWS CodeDeploy.
--
-- 'lastUsedTime', 'genericRevisionInfo_lastUsedTime' - When the revision was last used by AWS CodeDeploy.
newGenericRevisionInfo ::
  GenericRevisionInfo
newGenericRevisionInfo =
  GenericRevisionInfo'
    { registerTime = Core.Nothing,
      deploymentGroups = Core.Nothing,
      description = Core.Nothing,
      firstUsedTime = Core.Nothing,
      lastUsedTime = Core.Nothing
    }

-- | When the revision was registered with AWS CodeDeploy.
genericRevisionInfo_registerTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.UTCTime)
genericRevisionInfo_registerTime = Lens.lens (\GenericRevisionInfo' {registerTime} -> registerTime) (\s@GenericRevisionInfo' {} a -> s {registerTime = a} :: GenericRevisionInfo) Core.. Lens.mapping Core._Time

-- | The deployment groups for which this is the current target revision.
genericRevisionInfo_deploymentGroups :: Lens.Lens' GenericRevisionInfo (Core.Maybe [Core.Text])
genericRevisionInfo_deploymentGroups = Lens.lens (\GenericRevisionInfo' {deploymentGroups} -> deploymentGroups) (\s@GenericRevisionInfo' {} a -> s {deploymentGroups = a} :: GenericRevisionInfo) Core.. Lens.mapping Lens._Coerce

-- | A comment about the revision.
genericRevisionInfo_description :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.Text)
genericRevisionInfo_description = Lens.lens (\GenericRevisionInfo' {description} -> description) (\s@GenericRevisionInfo' {} a -> s {description = a} :: GenericRevisionInfo)

-- | When the revision was first used by AWS CodeDeploy.
genericRevisionInfo_firstUsedTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.UTCTime)
genericRevisionInfo_firstUsedTime = Lens.lens (\GenericRevisionInfo' {firstUsedTime} -> firstUsedTime) (\s@GenericRevisionInfo' {} a -> s {firstUsedTime = a} :: GenericRevisionInfo) Core.. Lens.mapping Core._Time

-- | When the revision was last used by AWS CodeDeploy.
genericRevisionInfo_lastUsedTime :: Lens.Lens' GenericRevisionInfo (Core.Maybe Core.UTCTime)
genericRevisionInfo_lastUsedTime = Lens.lens (\GenericRevisionInfo' {lastUsedTime} -> lastUsedTime) (\s@GenericRevisionInfo' {} a -> s {lastUsedTime = a} :: GenericRevisionInfo) Core.. Lens.mapping Core._Time

instance Core.FromJSON GenericRevisionInfo where
  parseJSON =
    Core.withObject
      "GenericRevisionInfo"
      ( \x ->
          GenericRevisionInfo'
            Core.<$> (x Core..:? "registerTime")
            Core.<*> (x Core..:? "deploymentGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "firstUsedTime")
            Core.<*> (x Core..:? "lastUsedTime")
      )

instance Core.Hashable GenericRevisionInfo

instance Core.NFData GenericRevisionInfo
