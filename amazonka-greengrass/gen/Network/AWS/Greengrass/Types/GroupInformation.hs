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
-- Module      : Network.AWS.Greengrass.Types.GroupInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupInformation where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a group.
--
-- /See:/ 'newGroupInformation' smart constructor.
data GroupInformation = GroupInformation'
  { -- | The time, in milliseconds since the epoch, when the group was created.
    creationTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the latest version associated with the group.
    latestVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the latest version associated with the group.
    latestVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the group was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { creationTimestamp =
        Prelude.Nothing,
      latestVersionArn = Prelude.Nothing,
      latestVersion = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the group was created.
groupInformation_creationTimestamp :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_creationTimestamp = Lens.lens (\GroupInformation' {creationTimestamp} -> creationTimestamp) (\s@GroupInformation' {} a -> s {creationTimestamp = a} :: GroupInformation)

-- | The ARN of the latest version associated with the group.
groupInformation_latestVersionArn :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_latestVersionArn = Lens.lens (\GroupInformation' {latestVersionArn} -> latestVersionArn) (\s@GroupInformation' {} a -> s {latestVersionArn = a} :: GroupInformation)

-- | The ID of the latest version associated with the group.
groupInformation_latestVersion :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_latestVersion = Lens.lens (\GroupInformation' {latestVersion} -> latestVersion) (\s@GroupInformation' {} a -> s {latestVersion = a} :: GroupInformation)

-- | The ARN of the group.
groupInformation_arn :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_arn = Lens.lens (\GroupInformation' {arn} -> arn) (\s@GroupInformation' {} a -> s {arn = a} :: GroupInformation)

-- | The ID of the group.
groupInformation_id :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_id = Lens.lens (\GroupInformation' {id} -> id) (\s@GroupInformation' {} a -> s {id = a} :: GroupInformation)

-- | The name of the group.
groupInformation_name :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_name = Lens.lens (\GroupInformation' {name} -> name) (\s@GroupInformation' {} a -> s {name = a} :: GroupInformation)

-- | The time, in milliseconds since the epoch, when the group was last
-- updated.
groupInformation_lastUpdatedTimestamp :: Lens.Lens' GroupInformation (Prelude.Maybe Prelude.Text)
groupInformation_lastUpdatedTimestamp = Lens.lens (\GroupInformation' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@GroupInformation' {} a -> s {lastUpdatedTimestamp = a} :: GroupInformation)

instance Prelude.FromJSON GroupInformation where
  parseJSON =
    Prelude.withObject
      "GroupInformation"
      ( \x ->
          GroupInformation'
            Prelude.<$> (x Prelude..:? "CreationTimestamp")
            Prelude.<*> (x Prelude..:? "LatestVersionArn")
            Prelude.<*> (x Prelude..:? "LatestVersion")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "LastUpdatedTimestamp")
      )

instance Prelude.Hashable GroupInformation

instance Prelude.NFData GroupInformation
