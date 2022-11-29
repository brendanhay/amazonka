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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The container path, mount options, and size (in MiB) of a tmpfs mount.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails'
  { -- | The absolute file path where the tmpfs volume is to be mounted.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | The maximum size (in MiB) of the tmpfs volume.
    size :: Prelude.Maybe Prelude.Int,
    -- | The list of tmpfs volume mount options.
    --
    -- Valid values: @\"defaults\"@ | @\"ro\"@ | @\"rw\"@ | @\"suid\"@ |
    -- @\"nosuid\"@ | @\"dev\"@ | @\"nodev\"@ |@ \"exec\"@ | @\"noexec\"@ |
    -- @\"sync\"@ | @\"async\"@ | @\"dirsync\"@ | @\"remount\"@ | @\"mand\"@ |
    -- @\"nomand\"@ | @\"atime\"@ | @\"noatime\"@ | @\"diratime\"@ |
    -- @\"nodiratime\"@ | @\"bind\"@ | @\"rbind\"@ | @\"unbindable\"@ |
    -- @\"runbindable\"@ | @\"private\"@ | @\"rprivate\"@ | @\"shared\"@ |
    -- @\"rshared\"@ | @\"slave\"@ | @\"rslave\"@ | @\"relatime\"@ |
    -- @\"norelatime\"@ | @\"strictatime\"@ | @\"nostrictatime\"@ |@ \"mode\"@
    -- | @\"uid\"@ | @\"gid\"@ | @\"nr_inodes\"@ |@ \"nr_blocks\"@ | @\"mpol\"@
    mountOptions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPath', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath' - The absolute file path where the tmpfs volume is to be mounted.
--
-- 'size', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size' - The maximum size (in MiB) of the tmpfs volume.
--
-- 'mountOptions', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions' - The list of tmpfs volume mount options.
--
-- Valid values: @\"defaults\"@ | @\"ro\"@ | @\"rw\"@ | @\"suid\"@ |
-- @\"nosuid\"@ | @\"dev\"@ | @\"nodev\"@ |@ \"exec\"@ | @\"noexec\"@ |
-- @\"sync\"@ | @\"async\"@ | @\"dirsync\"@ | @\"remount\"@ | @\"mand\"@ |
-- @\"nomand\"@ | @\"atime\"@ | @\"noatime\"@ | @\"diratime\"@ |
-- @\"nodiratime\"@ | @\"bind\"@ | @\"rbind\"@ | @\"unbindable\"@ |
-- @\"runbindable\"@ | @\"private\"@ | @\"rprivate\"@ | @\"shared\"@ |
-- @\"rshared\"@ | @\"slave\"@ | @\"rslave\"@ | @\"relatime\"@ |
-- @\"norelatime\"@ | @\"strictatime\"@ | @\"nostrictatime\"@ |@ \"mode\"@
-- | @\"uid\"@ | @\"gid\"@ | @\"nr_inodes\"@ |@ \"nr_blocks\"@ | @\"mpol\"@
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails'
    { containerPath =
        Prelude.Nothing,
      size =
        Prelude.Nothing,
      mountOptions =
        Prelude.Nothing
    }

-- | The absolute file path where the tmpfs volume is to be mounted.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_containerPath = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {containerPath} -> containerPath) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {} a -> s {containerPath = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails)

-- | The maximum size (in MiB) of the tmpfs volume.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_size = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {size} -> size) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {} a -> s {size = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails)

-- | The list of tmpfs volume mount options.
--
-- Valid values: @\"defaults\"@ | @\"ro\"@ | @\"rw\"@ | @\"suid\"@ |
-- @\"nosuid\"@ | @\"dev\"@ | @\"nodev\"@ |@ \"exec\"@ | @\"noexec\"@ |
-- @\"sync\"@ | @\"async\"@ | @\"dirsync\"@ | @\"remount\"@ | @\"mand\"@ |
-- @\"nomand\"@ | @\"atime\"@ | @\"noatime\"@ | @\"diratime\"@ |
-- @\"nodiratime\"@ | @\"bind\"@ | @\"rbind\"@ | @\"unbindable\"@ |
-- @\"runbindable\"@ | @\"private\"@ | @\"rprivate\"@ | @\"shared\"@ |
-- @\"rshared\"@ | @\"slave\"@ | @\"rslave\"@ | @\"relatime\"@ |
-- @\"norelatime\"@ | @\"strictatime\"@ | @\"nostrictatime\"@ |@ \"mode\"@
-- | @\"uid\"@ | @\"gid\"@ | @\"nr_inodes\"@ |@ \"nr_blocks\"@ | @\"mpol\"@
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails_mountOptions = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {mountOptions} -> mountOptions) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {} a -> s {mountOptions = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails'
            Prelude.<$> (x Core..:? "ContainerPath")
              Prelude.<*> (x Core..:? "Size")
              Prelude.<*> (x Core..:? "MountOptions" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {..} =
      _salt `Prelude.hashWithSalt` containerPath
        `Prelude.hashWithSalt` size
        `Prelude.hashWithSalt` mountOptions

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {..} =
      Prelude.rnf containerPath
        `Prelude.seq` Prelude.rnf size
        `Prelude.seq` Prelude.rnf mountOptions

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersTmpfsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ContainerPath" Core..=) Prelude.<$> containerPath,
              ("Size" Core..=) Prelude.<$> size,
              ("MountOptions" Core..=) Prelude.<$> mountOptions
            ]
        )
