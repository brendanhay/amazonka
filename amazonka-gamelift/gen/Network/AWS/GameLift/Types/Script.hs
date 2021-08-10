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
import qualified Network.AWS.Prelude as Prelude

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
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- that is assigned to a GameLift script resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift script ARN, the
    -- resource ID matches the /ScriptId/ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | The version that is associated with a build or script. Version strings
    -- do not need to be unique.
    version :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    storageLocation :: Prelude.Maybe S3Location,
    -- | The file size of the uploaded Realtime script, expressed in bytes. When
    -- files are uploaded from an S3 location, this value remains at \"0\".
    sizeOnDisk :: Prelude.Maybe Prelude.Natural,
    -- | A unique identifier for a Realtime script
    scriptId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { creationTime = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      sizeOnDisk = Prelude.Nothing,
      scriptId = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. The format is
-- a number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
script_creationTime :: Lens.Lens' Script (Prelude.Maybe Prelude.UTCTime)
script_creationTime = Lens.lens (\Script' {creationTime} -> creationTime) (\s@Script' {} a -> s {creationTime = a} :: Script) Prelude.. Lens.mapping Core._Time

-- | Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
script_scriptArn :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptArn = Lens.lens (\Script' {scriptArn} -> scriptArn) (\s@Script' {} a -> s {scriptArn = a} :: Script)

-- | The version that is associated with a build or script. Version strings
-- do not need to be unique.
script_version :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_version = Lens.lens (\Script' {version} -> version) (\s@Script' {} a -> s {version = a} :: Script)

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique.
script_name :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_name = Lens.lens (\Script' {name} -> name) (\s@Script' {} a -> s {name = a} :: Script)

-- | Undocumented member.
script_storageLocation :: Lens.Lens' Script (Prelude.Maybe S3Location)
script_storageLocation = Lens.lens (\Script' {storageLocation} -> storageLocation) (\s@Script' {} a -> s {storageLocation = a} :: Script)

-- | The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
script_sizeOnDisk :: Lens.Lens' Script (Prelude.Maybe Prelude.Natural)
script_sizeOnDisk = Lens.lens (\Script' {sizeOnDisk} -> sizeOnDisk) (\s@Script' {} a -> s {sizeOnDisk = a} :: Script)

-- | A unique identifier for a Realtime script
script_scriptId :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptId = Lens.lens (\Script' {scriptId} -> scriptId) (\s@Script' {} a -> s {scriptId = a} :: Script)

instance Core.FromJSON Script where
  parseJSON =
    Core.withObject
      "Script"
      ( \x ->
          Script'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ScriptArn")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "StorageLocation")
            Prelude.<*> (x Core..:? "SizeOnDisk")
            Prelude.<*> (x Core..:? "ScriptId")
      )

instance Prelude.Hashable Script

instance Prelude.NFData Script
