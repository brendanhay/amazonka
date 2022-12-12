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
-- Module      : Amazonka.GuardDuty.Types.Container
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.Container where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.SecurityContext
import Amazonka.GuardDuty.Types.VolumeMount
import qualified Amazonka.Prelude as Prelude

-- | Details of a container.
--
-- /See:/ 'newContainer' smart constructor.
data Container = Container'
  { -- | The container runtime (such as, Docker or containerd) used to run the
    -- container.
    containerRuntime :: Prelude.Maybe Prelude.Text,
    -- | Container ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Container image.
    image :: Prelude.Maybe Prelude.Text,
    -- | Part of the image name before the last slash. For example, imagePrefix
    -- for public.ecr.aws\/amazonlinux\/amazonlinux:latest would be
    -- public.ecr.aws\/amazonlinux. If the image name is relative and does not
    -- have a slash, this field is empty.
    imagePrefix :: Prelude.Maybe Prelude.Text,
    -- | Container name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Container security context.
    securityContext :: Prelude.Maybe SecurityContext,
    -- | Container volume mounts.
    volumeMounts :: Prelude.Maybe [VolumeMount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Container' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerRuntime', 'container_containerRuntime' - The container runtime (such as, Docker or containerd) used to run the
-- container.
--
-- 'id', 'container_id' - Container ID.
--
-- 'image', 'container_image' - Container image.
--
-- 'imagePrefix', 'container_imagePrefix' - Part of the image name before the last slash. For example, imagePrefix
-- for public.ecr.aws\/amazonlinux\/amazonlinux:latest would be
-- public.ecr.aws\/amazonlinux. If the image name is relative and does not
-- have a slash, this field is empty.
--
-- 'name', 'container_name' - Container name.
--
-- 'securityContext', 'container_securityContext' - Container security context.
--
-- 'volumeMounts', 'container_volumeMounts' - Container volume mounts.
newContainer ::
  Container
newContainer =
  Container'
    { containerRuntime = Prelude.Nothing,
      id = Prelude.Nothing,
      image = Prelude.Nothing,
      imagePrefix = Prelude.Nothing,
      name = Prelude.Nothing,
      securityContext = Prelude.Nothing,
      volumeMounts = Prelude.Nothing
    }

-- | The container runtime (such as, Docker or containerd) used to run the
-- container.
container_containerRuntime :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_containerRuntime = Lens.lens (\Container' {containerRuntime} -> containerRuntime) (\s@Container' {} a -> s {containerRuntime = a} :: Container)

-- | Container ID.
container_id :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_id = Lens.lens (\Container' {id} -> id) (\s@Container' {} a -> s {id = a} :: Container)

-- | Container image.
container_image :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_image = Lens.lens (\Container' {image} -> image) (\s@Container' {} a -> s {image = a} :: Container)

-- | Part of the image name before the last slash. For example, imagePrefix
-- for public.ecr.aws\/amazonlinux\/amazonlinux:latest would be
-- public.ecr.aws\/amazonlinux. If the image name is relative and does not
-- have a slash, this field is empty.
container_imagePrefix :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_imagePrefix = Lens.lens (\Container' {imagePrefix} -> imagePrefix) (\s@Container' {} a -> s {imagePrefix = a} :: Container)

-- | Container name.
container_name :: Lens.Lens' Container (Prelude.Maybe Prelude.Text)
container_name = Lens.lens (\Container' {name} -> name) (\s@Container' {} a -> s {name = a} :: Container)

-- | Container security context.
container_securityContext :: Lens.Lens' Container (Prelude.Maybe SecurityContext)
container_securityContext = Lens.lens (\Container' {securityContext} -> securityContext) (\s@Container' {} a -> s {securityContext = a} :: Container)

-- | Container volume mounts.
container_volumeMounts :: Lens.Lens' Container (Prelude.Maybe [VolumeMount])
container_volumeMounts = Lens.lens (\Container' {volumeMounts} -> volumeMounts) (\s@Container' {} a -> s {volumeMounts = a} :: Container) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Container where
  parseJSON =
    Data.withObject
      "Container"
      ( \x ->
          Container'
            Prelude.<$> (x Data..:? "containerRuntime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "image")
            Prelude.<*> (x Data..:? "imagePrefix")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "securityContext")
            Prelude.<*> (x Data..:? "volumeMounts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Container where
  hashWithSalt _salt Container' {..} =
    _salt `Prelude.hashWithSalt` containerRuntime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` image
      `Prelude.hashWithSalt` imagePrefix
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` securityContext
      `Prelude.hashWithSalt` volumeMounts

instance Prelude.NFData Container where
  rnf Container' {..} =
    Prelude.rnf containerRuntime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf image
      `Prelude.seq` Prelude.rnf imagePrefix
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf securityContext
      `Prelude.seq` Prelude.rnf volumeMounts
