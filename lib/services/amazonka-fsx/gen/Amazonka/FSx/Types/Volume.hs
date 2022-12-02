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
-- Module      : Amazonka.FSx.Types.Volume
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Volume where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import {-# SOURCE #-} Amazonka.FSx.Types.AdministrativeAction
import Amazonka.FSx.Types.LifecycleTransitionReason
import Amazonka.FSx.Types.OntapVolumeConfiguration
import Amazonka.FSx.Types.OpenZFSVolumeConfiguration
import Amazonka.FSx.Types.Tag
import Amazonka.FSx.Types.VolumeLifecycle
import Amazonka.FSx.Types.VolumeType
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon FSx for NetApp ONTAP or Amazon FSx for OpenZFS
-- volume.
--
-- /See:/ 'newVolume' smart constructor.
data Volume = Volume'
  { tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the volume.
    name :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle status of the volume.
    --
    -- -   @AVAILABLE@ - The volume is fully available for use.
    --
    -- -   @CREATED@ - The volume has been created.
    --
    -- -   @CREATING@ - Amazon FSx is creating the new volume.
    --
    -- -   @DELETING@ - Amazon FSx is deleting an existing volume.
    --
    -- -   @FAILED@ - Amazon FSx was unable to create the volume.
    --
    -- -   @MISCONFIGURED@ - The volume is in a failed but recoverable state.
    --
    -- -   @PENDING@ - Amazon FSx hasn\'t started creating the volume.
    lifecycle :: Prelude.Maybe VolumeLifecycle,
    -- | A list of administrative actions for the file system that are in process
    -- or waiting to be processed. Administrative actions describe changes to
    -- the Amazon FSx system that you initiated.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    -- | The configuration of an Amazon FSx for OpenZFS volume.
    openZFSConfiguration :: Prelude.Maybe OpenZFSVolumeConfiguration,
    -- | The type of the volume.
    volumeType :: Prelude.Maybe VolumeType,
    fileSystemId :: Prelude.Maybe Prelude.Text,
    ontapConfiguration :: Prelude.Maybe OntapVolumeConfiguration,
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The system-generated, unique ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    resourceARN :: Prelude.Maybe Prelude.Text,
    -- | The reason why the volume lifecycle status changed.
    lifecycleTransitionReason :: Prelude.Maybe LifecycleTransitionReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Volume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'volume_tags' - Undocumented member.
--
-- 'name', 'volume_name' - The name of the volume.
--
-- 'lifecycle', 'volume_lifecycle' - The lifecycle status of the volume.
--
-- -   @AVAILABLE@ - The volume is fully available for use.
--
-- -   @CREATED@ - The volume has been created.
--
-- -   @CREATING@ - Amazon FSx is creating the new volume.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing volume.
--
-- -   @FAILED@ - Amazon FSx was unable to create the volume.
--
-- -   @MISCONFIGURED@ - The volume is in a failed but recoverable state.
--
-- -   @PENDING@ - Amazon FSx hasn\'t started creating the volume.
--
-- 'administrativeActions', 'volume_administrativeActions' - A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system that you initiated.
--
-- 'openZFSConfiguration', 'volume_openZFSConfiguration' - The configuration of an Amazon FSx for OpenZFS volume.
--
-- 'volumeType', 'volume_volumeType' - The type of the volume.
--
-- 'fileSystemId', 'volume_fileSystemId' - Undocumented member.
--
-- 'ontapConfiguration', 'volume_ontapConfiguration' - Undocumented member.
--
-- 'creationTime', 'volume_creationTime' - Undocumented member.
--
-- 'volumeId', 'volume_volumeId' - The system-generated, unique ID of the volume.
--
-- 'resourceARN', 'volume_resourceARN' - Undocumented member.
--
-- 'lifecycleTransitionReason', 'volume_lifecycleTransitionReason' - The reason why the volume lifecycle status changed.
newVolume ::
  Volume
newVolume =
  Volume'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      administrativeActions = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      lifecycleTransitionReason = Prelude.Nothing
    }

-- | Undocumented member.
volume_tags :: Lens.Lens' Volume (Prelude.Maybe (Prelude.NonEmpty Tag))
volume_tags = Lens.lens (\Volume' {tags} -> tags) (\s@Volume' {} a -> s {tags = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | The name of the volume.
volume_name :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | The lifecycle status of the volume.
--
-- -   @AVAILABLE@ - The volume is fully available for use.
--
-- -   @CREATED@ - The volume has been created.
--
-- -   @CREATING@ - Amazon FSx is creating the new volume.
--
-- -   @DELETING@ - Amazon FSx is deleting an existing volume.
--
-- -   @FAILED@ - Amazon FSx was unable to create the volume.
--
-- -   @MISCONFIGURED@ - The volume is in a failed but recoverable state.
--
-- -   @PENDING@ - Amazon FSx hasn\'t started creating the volume.
volume_lifecycle :: Lens.Lens' Volume (Prelude.Maybe VolumeLifecycle)
volume_lifecycle = Lens.lens (\Volume' {lifecycle} -> lifecycle) (\s@Volume' {} a -> s {lifecycle = a} :: Volume)

-- | A list of administrative actions for the file system that are in process
-- or waiting to be processed. Administrative actions describe changes to
-- the Amazon FSx system that you initiated.
volume_administrativeActions :: Lens.Lens' Volume (Prelude.Maybe [AdministrativeAction])
volume_administrativeActions = Lens.lens (\Volume' {administrativeActions} -> administrativeActions) (\s@Volume' {} a -> s {administrativeActions = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of an Amazon FSx for OpenZFS volume.
volume_openZFSConfiguration :: Lens.Lens' Volume (Prelude.Maybe OpenZFSVolumeConfiguration)
volume_openZFSConfiguration = Lens.lens (\Volume' {openZFSConfiguration} -> openZFSConfiguration) (\s@Volume' {} a -> s {openZFSConfiguration = a} :: Volume)

-- | The type of the volume.
volume_volumeType :: Lens.Lens' Volume (Prelude.Maybe VolumeType)
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

-- | Undocumented member.
volume_fileSystemId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_fileSystemId = Lens.lens (\Volume' {fileSystemId} -> fileSystemId) (\s@Volume' {} a -> s {fileSystemId = a} :: Volume)

-- | Undocumented member.
volume_ontapConfiguration :: Lens.Lens' Volume (Prelude.Maybe OntapVolumeConfiguration)
volume_ontapConfiguration = Lens.lens (\Volume' {ontapConfiguration} -> ontapConfiguration) (\s@Volume' {} a -> s {ontapConfiguration = a} :: Volume)

-- | Undocumented member.
volume_creationTime :: Lens.Lens' Volume (Prelude.Maybe Prelude.UTCTime)
volume_creationTime = Lens.lens (\Volume' {creationTime} -> creationTime) (\s@Volume' {} a -> s {creationTime = a} :: Volume) Prelude.. Lens.mapping Data._Time

-- | The system-generated, unique ID of the volume.
volume_volumeId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | Undocumented member.
volume_resourceARN :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_resourceARN = Lens.lens (\Volume' {resourceARN} -> resourceARN) (\s@Volume' {} a -> s {resourceARN = a} :: Volume)

-- | The reason why the volume lifecycle status changed.
volume_lifecycleTransitionReason :: Lens.Lens' Volume (Prelude.Maybe LifecycleTransitionReason)
volume_lifecycleTransitionReason = Lens.lens (\Volume' {lifecycleTransitionReason} -> lifecycleTransitionReason) (\s@Volume' {} a -> s {lifecycleTransitionReason = a} :: Volume)

instance Data.FromJSON Volume where
  parseJSON =
    Data.withObject
      "Volume"
      ( \x ->
          Volume'
            Prelude.<$> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> ( x Data..:? "AdministrativeActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OpenZFSConfiguration")
            Prelude.<*> (x Data..:? "VolumeType")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "OntapConfiguration")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "LifecycleTransitionReason")
      )

instance Prelude.Hashable Volume where
  hashWithSalt _salt Volume' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` administrativeActions
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` volumeType
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` lifecycleTransitionReason

instance Prelude.NFData Volume where
  rnf Volume' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf administrativeActions
      `Prelude.seq` Prelude.rnf openZFSConfiguration
      `Prelude.seq` Prelude.rnf volumeType
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf ontapConfiguration
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf volumeId
      `Prelude.seq` Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf lifecycleTransitionReason
