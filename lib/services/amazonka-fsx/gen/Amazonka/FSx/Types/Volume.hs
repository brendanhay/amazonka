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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A list of administrative actions for the volume that are in process or
    -- waiting to be processed. Administrative actions describe changes to the
    -- volume that you have initiated using the @UpdateVolume@ action.
    administrativeActions :: Prelude.Maybe [AdministrativeAction],
    creationTime :: Prelude.Maybe Data.POSIX,
    fileSystemId :: Prelude.Maybe Prelude.Text,
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
    -- | The reason why the volume lifecycle status changed.
    lifecycleTransitionReason :: Prelude.Maybe LifecycleTransitionReason,
    -- | The name of the volume.
    name :: Prelude.Maybe Prelude.Text,
    ontapConfiguration :: Prelude.Maybe OntapVolumeConfiguration,
    -- | The configuration of an Amazon FSx for OpenZFS volume.
    openZFSConfiguration :: Prelude.Maybe OpenZFSVolumeConfiguration,
    resourceARN :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The system-generated, unique ID of the volume.
    volumeId :: Prelude.Maybe Prelude.Text,
    -- | The type of the volume.
    volumeType :: Prelude.Maybe VolumeType
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
-- 'administrativeActions', 'volume_administrativeActions' - A list of administrative actions for the volume that are in process or
-- waiting to be processed. Administrative actions describe changes to the
-- volume that you have initiated using the @UpdateVolume@ action.
--
-- 'creationTime', 'volume_creationTime' - Undocumented member.
--
-- 'fileSystemId', 'volume_fileSystemId' - Undocumented member.
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
-- 'lifecycleTransitionReason', 'volume_lifecycleTransitionReason' - The reason why the volume lifecycle status changed.
--
-- 'name', 'volume_name' - The name of the volume.
--
-- 'ontapConfiguration', 'volume_ontapConfiguration' - Undocumented member.
--
-- 'openZFSConfiguration', 'volume_openZFSConfiguration' - The configuration of an Amazon FSx for OpenZFS volume.
--
-- 'resourceARN', 'volume_resourceARN' - Undocumented member.
--
-- 'tags', 'volume_tags' - Undocumented member.
--
-- 'volumeId', 'volume_volumeId' - The system-generated, unique ID of the volume.
--
-- 'volumeType', 'volume_volumeType' - The type of the volume.
newVolume ::
  Volume
newVolume =
  Volume'
    { administrativeActions = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      lifecycle = Prelude.Nothing,
      lifecycleTransitionReason = Prelude.Nothing,
      name = Prelude.Nothing,
      ontapConfiguration = Prelude.Nothing,
      openZFSConfiguration = Prelude.Nothing,
      resourceARN = Prelude.Nothing,
      tags = Prelude.Nothing,
      volumeId = Prelude.Nothing,
      volumeType = Prelude.Nothing
    }

-- | A list of administrative actions for the volume that are in process or
-- waiting to be processed. Administrative actions describe changes to the
-- volume that you have initiated using the @UpdateVolume@ action.
volume_administrativeActions :: Lens.Lens' Volume (Prelude.Maybe [AdministrativeAction])
volume_administrativeActions = Lens.lens (\Volume' {administrativeActions} -> administrativeActions) (\s@Volume' {} a -> s {administrativeActions = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
volume_creationTime :: Lens.Lens' Volume (Prelude.Maybe Prelude.UTCTime)
volume_creationTime = Lens.lens (\Volume' {creationTime} -> creationTime) (\s@Volume' {} a -> s {creationTime = a} :: Volume) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
volume_fileSystemId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_fileSystemId = Lens.lens (\Volume' {fileSystemId} -> fileSystemId) (\s@Volume' {} a -> s {fileSystemId = a} :: Volume)

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

-- | The reason why the volume lifecycle status changed.
volume_lifecycleTransitionReason :: Lens.Lens' Volume (Prelude.Maybe LifecycleTransitionReason)
volume_lifecycleTransitionReason = Lens.lens (\Volume' {lifecycleTransitionReason} -> lifecycleTransitionReason) (\s@Volume' {} a -> s {lifecycleTransitionReason = a} :: Volume)

-- | The name of the volume.
volume_name :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_name = Lens.lens (\Volume' {name} -> name) (\s@Volume' {} a -> s {name = a} :: Volume)

-- | Undocumented member.
volume_ontapConfiguration :: Lens.Lens' Volume (Prelude.Maybe OntapVolumeConfiguration)
volume_ontapConfiguration = Lens.lens (\Volume' {ontapConfiguration} -> ontapConfiguration) (\s@Volume' {} a -> s {ontapConfiguration = a} :: Volume)

-- | The configuration of an Amazon FSx for OpenZFS volume.
volume_openZFSConfiguration :: Lens.Lens' Volume (Prelude.Maybe OpenZFSVolumeConfiguration)
volume_openZFSConfiguration = Lens.lens (\Volume' {openZFSConfiguration} -> openZFSConfiguration) (\s@Volume' {} a -> s {openZFSConfiguration = a} :: Volume)

-- | Undocumented member.
volume_resourceARN :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_resourceARN = Lens.lens (\Volume' {resourceARN} -> resourceARN) (\s@Volume' {} a -> s {resourceARN = a} :: Volume)

-- | Undocumented member.
volume_tags :: Lens.Lens' Volume (Prelude.Maybe (Prelude.NonEmpty Tag))
volume_tags = Lens.lens (\Volume' {tags} -> tags) (\s@Volume' {} a -> s {tags = a} :: Volume) Prelude.. Lens.mapping Lens.coerced

-- | The system-generated, unique ID of the volume.
volume_volumeId :: Lens.Lens' Volume (Prelude.Maybe Prelude.Text)
volume_volumeId = Lens.lens (\Volume' {volumeId} -> volumeId) (\s@Volume' {} a -> s {volumeId = a} :: Volume)

-- | The type of the volume.
volume_volumeType :: Lens.Lens' Volume (Prelude.Maybe VolumeType)
volume_volumeType = Lens.lens (\Volume' {volumeType} -> volumeType) (\s@Volume' {} a -> s {volumeType = a} :: Volume)

instance Data.FromJSON Volume where
  parseJSON =
    Data.withObject
      "Volume"
      ( \x ->
          Volume'
            Prelude.<$> ( x
                            Data..:? "AdministrativeActions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "LifecycleTransitionReason")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OntapConfiguration")
            Prelude.<*> (x Data..:? "OpenZFSConfiguration")
            Prelude.<*> (x Data..:? "ResourceARN")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "VolumeId")
            Prelude.<*> (x Data..:? "VolumeType")
      )

instance Prelude.Hashable Volume where
  hashWithSalt _salt Volume' {..} =
    _salt
      `Prelude.hashWithSalt` administrativeActions
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` lifecycleTransitionReason
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ontapConfiguration
      `Prelude.hashWithSalt` openZFSConfiguration
      `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` volumeId
      `Prelude.hashWithSalt` volumeType

instance Prelude.NFData Volume where
  rnf Volume' {..} =
    Prelude.rnf administrativeActions `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf fileSystemId `Prelude.seq`
          Prelude.rnf lifecycle `Prelude.seq`
            Prelude.rnf lifecycleTransitionReason `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf ontapConfiguration `Prelude.seq`
                  Prelude.rnf openZFSConfiguration `Prelude.seq`
                    Prelude.rnf resourceARN `Prelude.seq`
                      Prelude.rnf tags `Prelude.seq`
                        Prelude.rnf volumeId `Prelude.seq`
                          Prelude.rnf volumeType
