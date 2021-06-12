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
-- Module      : Network.AWS.GameLift.Types.Script
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.Script where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.S3Location
import qualified Network.AWS.Lens as Lens

-- | Properties describing a Realtime script.
--
-- __Related operations__
--
-- -   CreateScript
--
-- -   ListScripts
--
-- -   DescribeScript
--
-- -   UpdateScript
--
-- -   DeleteScript
--
-- /See:/ 'newScript' smart constructor.
data Script = Script'
  { -- | A time stamp indicating when this data object was created. The format is
    -- a number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift script resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift script ARN, the
    -- resource ID matches the /ScriptId/ value.
    scriptArn :: Core.Maybe Core.Text,
    -- | The version that is associated with a build or script. Version strings
    -- do not need to be unique.
    version :: Core.Maybe Core.Text,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique.
    name :: Core.Maybe Core.Text,
    storageLocation :: Core.Maybe S3Location,
    -- | The file size of the uploaded Realtime script, expressed in bytes. When
    -- files are uploaded from an S3 location, this value remains at \"0\".
    sizeOnDisk :: Core.Maybe Core.Natural,
    -- | A unique identifier for a Realtime script
    scriptId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Script' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'script_creationTime' - A time stamp indicating when this data object was created. The format is
-- a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'scriptArn', 'script_scriptArn' - Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
--
-- 'version', 'script_version' - The version that is associated with a build or script. Version strings
-- do not need to be unique.
--
-- 'name', 'script_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique.
--
-- 'storageLocation', 'script_storageLocation' - Undocumented member.
--
-- 'sizeOnDisk', 'script_sizeOnDisk' - The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
--
-- 'scriptId', 'script_scriptId' - A unique identifier for a Realtime script
newScript ::
  Script
newScript =
  Script'
    { creationTime = Core.Nothing,
      scriptArn = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      storageLocation = Core.Nothing,
      sizeOnDisk = Core.Nothing,
      scriptId = Core.Nothing
    }

-- | A time stamp indicating when this data object was created. The format is
-- a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
script_creationTime :: Lens.Lens' Script (Core.Maybe Core.UTCTime)
script_creationTime = Lens.lens (\Script' {creationTime} -> creationTime) (\s@Script' {} a -> s {creationTime = a} :: Script) Core.. Lens.mapping Core._Time

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
script_scriptArn :: Lens.Lens' Script (Core.Maybe Core.Text)
script_scriptArn = Lens.lens (\Script' {scriptArn} -> scriptArn) (\s@Script' {} a -> s {scriptArn = a} :: Script)

-- | The version that is associated with a build or script. Version strings
-- do not need to be unique.
script_version :: Lens.Lens' Script (Core.Maybe Core.Text)
script_version = Lens.lens (\Script' {version} -> version) (\s@Script' {} a -> s {version = a} :: Script)

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique.
script_name :: Lens.Lens' Script (Core.Maybe Core.Text)
script_name = Lens.lens (\Script' {name} -> name) (\s@Script' {} a -> s {name = a} :: Script)

-- | Undocumented member.
script_storageLocation :: Lens.Lens' Script (Core.Maybe S3Location)
script_storageLocation = Lens.lens (\Script' {storageLocation} -> storageLocation) (\s@Script' {} a -> s {storageLocation = a} :: Script)

-- | The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
script_sizeOnDisk :: Lens.Lens' Script (Core.Maybe Core.Natural)
script_sizeOnDisk = Lens.lens (\Script' {sizeOnDisk} -> sizeOnDisk) (\s@Script' {} a -> s {sizeOnDisk = a} :: Script)

-- | A unique identifier for a Realtime script
script_scriptId :: Lens.Lens' Script (Core.Maybe Core.Text)
script_scriptId = Lens.lens (\Script' {scriptId} -> scriptId) (\s@Script' {} a -> s {scriptId = a} :: Script)

instance Core.FromJSON Script where
  parseJSON =
    Core.withObject
      "Script"
      ( \x ->
          Script'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ScriptArn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "StorageLocation")
            Core.<*> (x Core..:? "SizeOnDisk")
            Core.<*> (x Core..:? "ScriptId")
      )

instance Core.Hashable Script

instance Core.NFData Script
