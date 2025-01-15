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
-- Module      : Amazonka.Batch.Types.EksContainerVolumeMount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.EksContainerVolumeMount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The volume mounts for a container for an Amazon EKS job. For more
-- information about volumes and volume mounts in Kubernetes, see
-- <https://kubernetes.io/docs/concepts/storage/volumes/ Volumes> in the
-- /Kubernetes documentation/.
--
-- /See:/ 'newEksContainerVolumeMount' smart constructor.
data EksContainerVolumeMount = EksContainerVolumeMount'
  { -- | The path on the container where the volume is mounted.
    mountPath :: Prelude.Maybe Prelude.Text,
    -- | The name the volume mount. This must match the name of one of the
    -- volumes in the pod.
    name :: Prelude.Maybe Prelude.Text,
    -- | If this value is @true@, the container has read-only access to the
    -- volume. Otherwise, the container can write to the volume. The default
    -- value is @false@.
    readOnly :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksContainerVolumeMount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountPath', 'eksContainerVolumeMount_mountPath' - The path on the container where the volume is mounted.
--
-- 'name', 'eksContainerVolumeMount_name' - The name the volume mount. This must match the name of one of the
-- volumes in the pod.
--
-- 'readOnly', 'eksContainerVolumeMount_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
newEksContainerVolumeMount ::
  EksContainerVolumeMount
newEksContainerVolumeMount =
  EksContainerVolumeMount'
    { mountPath =
        Prelude.Nothing,
      name = Prelude.Nothing,
      readOnly = Prelude.Nothing
    }

-- | The path on the container where the volume is mounted.
eksContainerVolumeMount_mountPath :: Lens.Lens' EksContainerVolumeMount (Prelude.Maybe Prelude.Text)
eksContainerVolumeMount_mountPath = Lens.lens (\EksContainerVolumeMount' {mountPath} -> mountPath) (\s@EksContainerVolumeMount' {} a -> s {mountPath = a} :: EksContainerVolumeMount)

-- | The name the volume mount. This must match the name of one of the
-- volumes in the pod.
eksContainerVolumeMount_name :: Lens.Lens' EksContainerVolumeMount (Prelude.Maybe Prelude.Text)
eksContainerVolumeMount_name = Lens.lens (\EksContainerVolumeMount' {name} -> name) (\s@EksContainerVolumeMount' {} a -> s {name = a} :: EksContainerVolumeMount)

-- | If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
eksContainerVolumeMount_readOnly :: Lens.Lens' EksContainerVolumeMount (Prelude.Maybe Prelude.Bool)
eksContainerVolumeMount_readOnly = Lens.lens (\EksContainerVolumeMount' {readOnly} -> readOnly) (\s@EksContainerVolumeMount' {} a -> s {readOnly = a} :: EksContainerVolumeMount)

instance Data.FromJSON EksContainerVolumeMount where
  parseJSON =
    Data.withObject
      "EksContainerVolumeMount"
      ( \x ->
          EksContainerVolumeMount'
            Prelude.<$> (x Data..:? "mountPath")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "readOnly")
      )

instance Prelude.Hashable EksContainerVolumeMount where
  hashWithSalt _salt EksContainerVolumeMount' {..} =
    _salt
      `Prelude.hashWithSalt` mountPath
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` readOnly

instance Prelude.NFData EksContainerVolumeMount where
  rnf EksContainerVolumeMount' {..} =
    Prelude.rnf mountPath `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf readOnly

instance Data.ToJSON EksContainerVolumeMount where
  toJSON EksContainerVolumeMount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mountPath" Data..=) Prelude.<$> mountPath,
            ("name" Data..=) Prelude.<$> name,
            ("readOnly" Data..=) Prelude.<$> readOnly
          ]
      )
