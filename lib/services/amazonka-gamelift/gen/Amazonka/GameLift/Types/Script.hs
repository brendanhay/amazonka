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
-- Module      : Amazonka.GameLift.Types.Script
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.Script where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | Properties describing a Realtime script.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newScript' smart constructor.
data Script = Script'
  { -- | A time stamp indicating when this data object was created. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A descriptive label that is associated with a script. Script names do
    -- not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift script resource and uniquely identifies
    -- it. ARNs are unique across all Regions. In a GameLift script ARN, the
    -- resource ID matches the /ScriptId/ value.
    scriptArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the Realtime script
    scriptId :: Prelude.Maybe Prelude.Text,
    -- | The file size of the uploaded Realtime script, expressed in bytes. When
    -- files are uploaded from an S3 location, this value remains at \"0\".
    sizeOnDisk :: Prelude.Maybe Prelude.Natural,
    -- | The location of the Amazon S3 bucket where a zipped file containing your
    -- Realtime scripts is stored. The storage location must specify the Amazon
    -- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
    -- allows Amazon GameLift to access the Amazon S3 storage location. The S3
    -- bucket must be in the same Region where you want to create a new script.
    -- By default, Amazon GameLift uploads the latest version of the zip file;
    -- if you have S3 object versioning turned on, you can use the
    -- @ObjectVersion@ parameter to specify an earlier version.
    storageLocation :: Prelude.Maybe S3Location,
    -- | Version information associated with a build or script. Version strings
    -- do not need to be unique.
    version :: Prelude.Maybe Prelude.Text
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
-- 'creationTime', 'script_creationTime' - A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'name', 'script_name' - A descriptive label that is associated with a script. Script names do
-- not need to be unique.
--
-- 'scriptArn', 'script_scriptArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
--
-- 'scriptId', 'script_scriptId' - A unique identifier for the Realtime script
--
-- 'sizeOnDisk', 'script_sizeOnDisk' - The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
--
-- 'storageLocation', 'script_storageLocation' - The location of the Amazon S3 bucket where a zipped file containing your
-- Realtime scripts is stored. The storage location must specify the Amazon
-- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
-- allows Amazon GameLift to access the Amazon S3 storage location. The S3
-- bucket must be in the same Region where you want to create a new script.
-- By default, Amazon GameLift uploads the latest version of the zip file;
-- if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version.
--
-- 'version', 'script_version' - Version information associated with a build or script. Version strings
-- do not need to be unique.
newScript ::
  Script
newScript =
  Script'
    { creationTime = Prelude.Nothing,
      name = Prelude.Nothing,
      scriptArn = Prelude.Nothing,
      scriptId = Prelude.Nothing,
      sizeOnDisk = Prelude.Nothing,
      storageLocation = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A time stamp indicating when this data object was created. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
script_creationTime :: Lens.Lens' Script (Prelude.Maybe Prelude.UTCTime)
script_creationTime = Lens.lens (\Script' {creationTime} -> creationTime) (\s@Script' {} a -> s {creationTime = a} :: Script) Prelude.. Lens.mapping Data._Time

-- | A descriptive label that is associated with a script. Script names do
-- not need to be unique.
script_name :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_name = Lens.lens (\Script' {name} -> name) (\s@Script' {} a -> s {name = a} :: Script)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift script resource and uniquely identifies
-- it. ARNs are unique across all Regions. In a GameLift script ARN, the
-- resource ID matches the /ScriptId/ value.
script_scriptArn :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptArn = Lens.lens (\Script' {scriptArn} -> scriptArn) (\s@Script' {} a -> s {scriptArn = a} :: Script)

-- | A unique identifier for the Realtime script
script_scriptId :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_scriptId = Lens.lens (\Script' {scriptId} -> scriptId) (\s@Script' {} a -> s {scriptId = a} :: Script)

-- | The file size of the uploaded Realtime script, expressed in bytes. When
-- files are uploaded from an S3 location, this value remains at \"0\".
script_sizeOnDisk :: Lens.Lens' Script (Prelude.Maybe Prelude.Natural)
script_sizeOnDisk = Lens.lens (\Script' {sizeOnDisk} -> sizeOnDisk) (\s@Script' {} a -> s {sizeOnDisk = a} :: Script)

-- | The location of the Amazon S3 bucket where a zipped file containing your
-- Realtime scripts is stored. The storage location must specify the Amazon
-- S3 bucket name, the zip file name (the \"key\"), and a role ARN that
-- allows Amazon GameLift to access the Amazon S3 storage location. The S3
-- bucket must be in the same Region where you want to create a new script.
-- By default, Amazon GameLift uploads the latest version of the zip file;
-- if you have S3 object versioning turned on, you can use the
-- @ObjectVersion@ parameter to specify an earlier version.
script_storageLocation :: Lens.Lens' Script (Prelude.Maybe S3Location)
script_storageLocation = Lens.lens (\Script' {storageLocation} -> storageLocation) (\s@Script' {} a -> s {storageLocation = a} :: Script)

-- | Version information associated with a build or script. Version strings
-- do not need to be unique.
script_version :: Lens.Lens' Script (Prelude.Maybe Prelude.Text)
script_version = Lens.lens (\Script' {version} -> version) (\s@Script' {} a -> s {version = a} :: Script)

instance Data.FromJSON Script where
  parseJSON =
    Data.withObject
      "Script"
      ( \x ->
          Script'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ScriptArn")
            Prelude.<*> (x Data..:? "ScriptId")
            Prelude.<*> (x Data..:? "SizeOnDisk")
            Prelude.<*> (x Data..:? "StorageLocation")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable Script where
  hashWithSalt _salt Script' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scriptArn
      `Prelude.hashWithSalt` scriptId
      `Prelude.hashWithSalt` sizeOnDisk
      `Prelude.hashWithSalt` storageLocation
      `Prelude.hashWithSalt` version

instance Prelude.NFData Script where
  rnf Script' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf scriptArn
      `Prelude.seq` Prelude.rnf scriptId
      `Prelude.seq` Prelude.rnf sizeOnDisk
      `Prelude.seq` Prelude.rnf storageLocation
      `Prelude.seq` Prelude.rnf version
