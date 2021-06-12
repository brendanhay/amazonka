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
-- Module      : Network.AWS.Greengrass.Types.VersionInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.VersionInformation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a version.
--
-- /See:/ 'newVersionInformation' smart constructor.
data VersionInformation = VersionInformation'
  { -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VersionInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'versionInformation_creationTimestamp' - The time, in milliseconds since the epoch, when the version was created.
--
-- 'arn', 'versionInformation_arn' - The ARN of the version.
--
-- 'id', 'versionInformation_id' - The ID of the parent definition that the version is associated with.
--
-- 'version', 'versionInformation_version' - The ID of the version.
newVersionInformation ::
  VersionInformation
newVersionInformation =
  VersionInformation'
    { creationTimestamp =
        Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing
    }

-- | The time, in milliseconds since the epoch, when the version was created.
versionInformation_creationTimestamp :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
versionInformation_creationTimestamp = Lens.lens (\VersionInformation' {creationTimestamp} -> creationTimestamp) (\s@VersionInformation' {} a -> s {creationTimestamp = a} :: VersionInformation)

-- | The ARN of the version.
versionInformation_arn :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
versionInformation_arn = Lens.lens (\VersionInformation' {arn} -> arn) (\s@VersionInformation' {} a -> s {arn = a} :: VersionInformation)

-- | The ID of the parent definition that the version is associated with.
versionInformation_id :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
versionInformation_id = Lens.lens (\VersionInformation' {id} -> id) (\s@VersionInformation' {} a -> s {id = a} :: VersionInformation)

-- | The ID of the version.
versionInformation_version :: Lens.Lens' VersionInformation (Core.Maybe Core.Text)
versionInformation_version = Lens.lens (\VersionInformation' {version} -> version) (\s@VersionInformation' {} a -> s {version = a} :: VersionInformation)

instance Core.FromJSON VersionInformation where
  parseJSON =
    Core.withObject
      "VersionInformation"
      ( \x ->
          VersionInformation'
            Core.<$> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
      )

instance Core.Hashable VersionInformation

instance Core.NFData VersionInformation
