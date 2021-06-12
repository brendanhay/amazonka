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
-- Module      : Network.AWS.Greengrass.Types.GroupInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a group.
--
-- /See:/ 'newGroupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { -- | The time, in milliseconds since the epoch, when the group was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the group.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the group.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the group.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the group.
    id :: Core.Maybe Core.Text,
    -- | The name of the group.
    name :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the group was last
    -- updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GroupInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'groupInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the group was created.
--
-- 'latestVersionArn', 'groupInformation_latestVersionArn' - The ARN of the latest version associated with the group.
--
-- 'latestVersion', 'groupInformation_latestVersion' - The ID of the latest version associated with the group.
--
-- 'arn', 'groupInformation_arn' - The ARN of the group.
--
-- 'id', 'groupInformation_id' - The ID of the group.
--
-- 'name', 'groupInformation_name' - The name of the group.
--
-- 'lastUpdatedTimestamp', 'groupInformation_lastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the group was last
-- updated.
newGroupInformation ::
  GroupInformation
newGroupInformation =
  GroupInformation'
    { creationTimestamp = Core.Nothing,
      latestVersionArn = Core.Nothing,
      latestVersion = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing
    }

-- | The time, in milliseconds since the epoch, when the group was created.
groupInformation_creationTimestamp :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_creationTimestamp = Lens.lens (\GroupInformation' {creationTimestamp} -> creationTimestamp) (\s@GroupInformation' {} a -> s {creationTimestamp = a} :: GroupInformation)

-- | The ARN of the latest version associated with the group.
groupInformation_latestVersionArn :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_latestVersionArn = Lens.lens (\GroupInformation' {latestVersionArn} -> latestVersionArn) (\s@GroupInformation' {} a -> s {latestVersionArn = a} :: GroupInformation)

-- | The ID of the latest version associated with the group.
groupInformation_latestVersion :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_latestVersion = Lens.lens (\GroupInformation' {latestVersion} -> latestVersion) (\s@GroupInformation' {} a -> s {latestVersion = a} :: GroupInformation)

-- | The ARN of the group.
groupInformation_arn :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_arn = Lens.lens (\GroupInformation' {arn} -> arn) (\s@GroupInformation' {} a -> s {arn = a} :: GroupInformation)

-- | The ID of the group.
groupInformation_id :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_id = Lens.lens (\GroupInformation' {id} -> id) (\s@GroupInformation' {} a -> s {id = a} :: GroupInformation)

-- | The name of the group.
groupInformation_name :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_name = Lens.lens (\GroupInformation' {name} -> name) (\s@GroupInformation' {} a -> s {name = a} :: GroupInformation)

-- | The time, in milliseconds since the epoch, when the group was last
-- updated.
groupInformation_lastUpdatedTimestamp :: Lens.Lens' GroupInformation (Core.Maybe Core.Text)
groupInformation_lastUpdatedTimestamp = Lens.lens (\GroupInformation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GroupInformation' {} a -> s {lastUpdatedTimestamp = a} :: GroupInformation)

instance Core.FromJSON GroupInformation where
  parseJSON =
    Core.withObject
      "GroupInformation"
      ( \x ->
          GroupInformation'
            Core.<$> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
      )

instance Core.Hashable GroupInformation

instance Core.NFData GroupInformation
