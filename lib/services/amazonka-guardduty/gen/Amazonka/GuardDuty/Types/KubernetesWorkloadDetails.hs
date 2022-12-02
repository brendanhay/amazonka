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
-- Module      : Amazonka.GuardDuty.Types.KubernetesWorkloadDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesWorkloadDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Container
import Amazonka.GuardDuty.Types.Volume
import qualified Amazonka.Prelude as Prelude

-- | Details about the Kubernetes workload involved in a Kubernetes finding.
--
-- /See:/ 'newKubernetesWorkloadDetails' smart constructor.
data KubernetesWorkloadDetails = KubernetesWorkloadDetails'
  { -- | Kubernetes workload name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Kubernetes workload type (e.g. Pod, Deployment, etc.).
    type' :: Prelude.Maybe Prelude.Text,
    -- | Containers running as part of the Kubernetes workload.
    containers :: Prelude.Maybe [Container],
    -- | Kubernetes workload ID.
    uid :: Prelude.Maybe Prelude.Text,
    -- | Volumes used by the Kubernetes workload.
    volumes :: Prelude.Maybe [Volume],
    -- | Kubernetes namespace that the workload is part of.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | Whether the hostNetwork flag is enabled for the pods included in the
    -- workload.
    hostNetwork :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesWorkloadDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'kubernetesWorkloadDetails_name' - Kubernetes workload name.
--
-- 'type'', 'kubernetesWorkloadDetails_type' - Kubernetes workload type (e.g. Pod, Deployment, etc.).
--
-- 'containers', 'kubernetesWorkloadDetails_containers' - Containers running as part of the Kubernetes workload.
--
-- 'uid', 'kubernetesWorkloadDetails_uid' - Kubernetes workload ID.
--
-- 'volumes', 'kubernetesWorkloadDetails_volumes' - Volumes used by the Kubernetes workload.
--
-- 'namespace', 'kubernetesWorkloadDetails_namespace' - Kubernetes namespace that the workload is part of.
--
-- 'hostNetwork', 'kubernetesWorkloadDetails_hostNetwork' - Whether the hostNetwork flag is enabled for the pods included in the
-- workload.
newKubernetesWorkloadDetails ::
  KubernetesWorkloadDetails
newKubernetesWorkloadDetails =
  KubernetesWorkloadDetails'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      containers = Prelude.Nothing,
      uid = Prelude.Nothing,
      volumes = Prelude.Nothing,
      namespace = Prelude.Nothing,
      hostNetwork = Prelude.Nothing
    }

-- | Kubernetes workload name.
kubernetesWorkloadDetails_name :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe Prelude.Text)
kubernetesWorkloadDetails_name = Lens.lens (\KubernetesWorkloadDetails' {name} -> name) (\s@KubernetesWorkloadDetails' {} a -> s {name = a} :: KubernetesWorkloadDetails)

-- | Kubernetes workload type (e.g. Pod, Deployment, etc.).
kubernetesWorkloadDetails_type :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe Prelude.Text)
kubernetesWorkloadDetails_type = Lens.lens (\KubernetesWorkloadDetails' {type'} -> type') (\s@KubernetesWorkloadDetails' {} a -> s {type' = a} :: KubernetesWorkloadDetails)

-- | Containers running as part of the Kubernetes workload.
kubernetesWorkloadDetails_containers :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe [Container])
kubernetesWorkloadDetails_containers = Lens.lens (\KubernetesWorkloadDetails' {containers} -> containers) (\s@KubernetesWorkloadDetails' {} a -> s {containers = a} :: KubernetesWorkloadDetails) Prelude.. Lens.mapping Lens.coerced

-- | Kubernetes workload ID.
kubernetesWorkloadDetails_uid :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe Prelude.Text)
kubernetesWorkloadDetails_uid = Lens.lens (\KubernetesWorkloadDetails' {uid} -> uid) (\s@KubernetesWorkloadDetails' {} a -> s {uid = a} :: KubernetesWorkloadDetails)

-- | Volumes used by the Kubernetes workload.
kubernetesWorkloadDetails_volumes :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe [Volume])
kubernetesWorkloadDetails_volumes = Lens.lens (\KubernetesWorkloadDetails' {volumes} -> volumes) (\s@KubernetesWorkloadDetails' {} a -> s {volumes = a} :: KubernetesWorkloadDetails) Prelude.. Lens.mapping Lens.coerced

-- | Kubernetes namespace that the workload is part of.
kubernetesWorkloadDetails_namespace :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe Prelude.Text)
kubernetesWorkloadDetails_namespace = Lens.lens (\KubernetesWorkloadDetails' {namespace} -> namespace) (\s@KubernetesWorkloadDetails' {} a -> s {namespace = a} :: KubernetesWorkloadDetails)

-- | Whether the hostNetwork flag is enabled for the pods included in the
-- workload.
kubernetesWorkloadDetails_hostNetwork :: Lens.Lens' KubernetesWorkloadDetails (Prelude.Maybe Prelude.Bool)
kubernetesWorkloadDetails_hostNetwork = Lens.lens (\KubernetesWorkloadDetails' {hostNetwork} -> hostNetwork) (\s@KubernetesWorkloadDetails' {} a -> s {hostNetwork = a} :: KubernetesWorkloadDetails)

instance Data.FromJSON KubernetesWorkloadDetails where
  parseJSON =
    Data.withObject
      "KubernetesWorkloadDetails"
      ( \x ->
          KubernetesWorkloadDetails'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "containers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "uid")
            Prelude.<*> (x Data..:? "volumes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namespace")
            Prelude.<*> (x Data..:? "hostNetwork")
      )

instance Prelude.Hashable KubernetesWorkloadDetails where
  hashWithSalt _salt KubernetesWorkloadDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` volumes
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` hostNetwork

instance Prelude.NFData KubernetesWorkloadDetails where
  rnf KubernetesWorkloadDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf uid
      `Prelude.seq` Prelude.rnf volumes
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf hostNetwork
