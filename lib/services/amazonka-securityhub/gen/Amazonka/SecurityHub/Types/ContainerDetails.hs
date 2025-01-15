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
-- Module      : Amazonka.SecurityHub.Types.ContainerDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ContainerDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.VolumeMount

-- | Container details related to a finding.
--
-- /See:/ 'newContainerDetails' smart constructor.
data ContainerDetails = ContainerDetails'
  { -- | The runtime of the container.
    containerRuntime :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the container image related to a finding.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The name of the container image related to a finding.
    imageName :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the container started.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    launchedAt :: Prelude.Maybe Prelude.Text,
    -- | The name of the container related to a finding.
    name :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is @true@, the container is given elevated
    -- privileges on the host container instance (similar to the root user).
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | Provides information about the mounting of a volume in a container.
    volumeMounts :: Prelude.Maybe [VolumeMount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRuntime', 'containerDetails_containerRuntime' - The runtime of the container.
--
-- 'imageId', 'containerDetails_imageId' - The identifier of the container image related to a finding.
--
-- 'imageName', 'containerDetails_imageName' - The name of the container image related to a finding.
--
-- 'launchedAt', 'containerDetails_launchedAt' - Indicates when the container started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'name', 'containerDetails_name' - The name of the container related to a finding.
--
-- 'privileged', 'containerDetails_privileged' - When this parameter is @true@, the container is given elevated
-- privileges on the host container instance (similar to the root user).
--
-- 'volumeMounts', 'containerDetails_volumeMounts' - Provides information about the mounting of a volume in a container.
newContainerDetails ::
  ContainerDetails
newContainerDetails =
  ContainerDetails'
    { containerRuntime =
        Prelude.Nothing,
      imageId = Prelude.Nothing,
      imageName = Prelude.Nothing,
      launchedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      privileged = Prelude.Nothing,
      volumeMounts = Prelude.Nothing
    }

-- | The runtime of the container.
containerDetails_containerRuntime :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_containerRuntime = Lens.lens (\ContainerDetails' {containerRuntime} -> containerRuntime) (\s@ContainerDetails' {} a -> s {containerRuntime = a} :: ContainerDetails)

-- | The identifier of the container image related to a finding.
containerDetails_imageId :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_imageId = Lens.lens (\ContainerDetails' {imageId} -> imageId) (\s@ContainerDetails' {} a -> s {imageId = a} :: ContainerDetails)

-- | The name of the container image related to a finding.
containerDetails_imageName :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_imageName = Lens.lens (\ContainerDetails' {imageName} -> imageName) (\s@ContainerDetails' {} a -> s {imageName = a} :: ContainerDetails)

-- | Indicates when the container started.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
containerDetails_launchedAt :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_launchedAt = Lens.lens (\ContainerDetails' {launchedAt} -> launchedAt) (\s@ContainerDetails' {} a -> s {launchedAt = a} :: ContainerDetails)

-- | The name of the container related to a finding.
containerDetails_name :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Text)
containerDetails_name = Lens.lens (\ContainerDetails' {name} -> name) (\s@ContainerDetails' {} a -> s {name = a} :: ContainerDetails)

-- | When this parameter is @true@, the container is given elevated
-- privileges on the host container instance (similar to the root user).
containerDetails_privileged :: Lens.Lens' ContainerDetails (Prelude.Maybe Prelude.Bool)
containerDetails_privileged = Lens.lens (\ContainerDetails' {privileged} -> privileged) (\s@ContainerDetails' {} a -> s {privileged = a} :: ContainerDetails)

-- | Provides information about the mounting of a volume in a container.
containerDetails_volumeMounts :: Lens.Lens' ContainerDetails (Prelude.Maybe [VolumeMount])
containerDetails_volumeMounts = Lens.lens (\ContainerDetails' {volumeMounts} -> volumeMounts) (\s@ContainerDetails' {} a -> s {volumeMounts = a} :: ContainerDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ContainerDetails where
  parseJSON =
    Data.withObject
      "ContainerDetails"
      ( \x ->
          ContainerDetails'
            Prelude.<$> (x Data..:? "ContainerRuntime")
            Prelude.<*> (x Data..:? "ImageId")
            Prelude.<*> (x Data..:? "ImageName")
            Prelude.<*> (x Data..:? "LaunchedAt")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Privileged")
            Prelude.<*> (x Data..:? "VolumeMounts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ContainerDetails where
  hashWithSalt _salt ContainerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` containerRuntime
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` imageName
      `Prelude.hashWithSalt` launchedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` privileged
      `Prelude.hashWithSalt` volumeMounts

instance Prelude.NFData ContainerDetails where
  rnf ContainerDetails' {..} =
    Prelude.rnf containerRuntime `Prelude.seq`
      Prelude.rnf imageId `Prelude.seq`
        Prelude.rnf imageName `Prelude.seq`
          Prelude.rnf launchedAt `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf privileged `Prelude.seq`
                Prelude.rnf volumeMounts

instance Data.ToJSON ContainerDetails where
  toJSON ContainerDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerRuntime" Data..=)
              Prelude.<$> containerRuntime,
            ("ImageId" Data..=) Prelude.<$> imageId,
            ("ImageName" Data..=) Prelude.<$> imageName,
            ("LaunchedAt" Data..=) Prelude.<$> launchedAt,
            ("Name" Data..=) Prelude.<$> name,
            ("Privileged" Data..=) Prelude.<$> privileged,
            ("VolumeMounts" Data..=) Prelude.<$> volumeMounts
          ]
      )
