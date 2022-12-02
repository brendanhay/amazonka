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
-- Module      : Amazonka.GuardDuty.Types.KubernetesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.KubernetesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.KubernetesUserDetails
import Amazonka.GuardDuty.Types.KubernetesWorkloadDetails
import qualified Amazonka.Prelude as Prelude

-- | Details about Kubernetes resources such as a Kubernetes user or workload
-- resource involved in a Kubernetes finding.
--
-- /See:/ 'newKubernetesDetails' smart constructor.
data KubernetesDetails = KubernetesDetails'
  { -- | Details about the Kubernetes user involved in a Kubernetes finding.
    kubernetesUserDetails :: Prelude.Maybe KubernetesUserDetails,
    -- | Details about the Kubernetes workload involved in a Kubernetes finding.
    kubernetesWorkloadDetails :: Prelude.Maybe KubernetesWorkloadDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KubernetesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kubernetesUserDetails', 'kubernetesDetails_kubernetesUserDetails' - Details about the Kubernetes user involved in a Kubernetes finding.
--
-- 'kubernetesWorkloadDetails', 'kubernetesDetails_kubernetesWorkloadDetails' - Details about the Kubernetes workload involved in a Kubernetes finding.
newKubernetesDetails ::
  KubernetesDetails
newKubernetesDetails =
  KubernetesDetails'
    { kubernetesUserDetails =
        Prelude.Nothing,
      kubernetesWorkloadDetails = Prelude.Nothing
    }

-- | Details about the Kubernetes user involved in a Kubernetes finding.
kubernetesDetails_kubernetesUserDetails :: Lens.Lens' KubernetesDetails (Prelude.Maybe KubernetesUserDetails)
kubernetesDetails_kubernetesUserDetails = Lens.lens (\KubernetesDetails' {kubernetesUserDetails} -> kubernetesUserDetails) (\s@KubernetesDetails' {} a -> s {kubernetesUserDetails = a} :: KubernetesDetails)

-- | Details about the Kubernetes workload involved in a Kubernetes finding.
kubernetesDetails_kubernetesWorkloadDetails :: Lens.Lens' KubernetesDetails (Prelude.Maybe KubernetesWorkloadDetails)
kubernetesDetails_kubernetesWorkloadDetails = Lens.lens (\KubernetesDetails' {kubernetesWorkloadDetails} -> kubernetesWorkloadDetails) (\s@KubernetesDetails' {} a -> s {kubernetesWorkloadDetails = a} :: KubernetesDetails)

instance Data.FromJSON KubernetesDetails where
  parseJSON =
    Data.withObject
      "KubernetesDetails"
      ( \x ->
          KubernetesDetails'
            Prelude.<$> (x Data..:? "kubernetesUserDetails")
            Prelude.<*> (x Data..:? "kubernetesWorkloadDetails")
      )

instance Prelude.Hashable KubernetesDetails where
  hashWithSalt _salt KubernetesDetails' {..} =
    _salt `Prelude.hashWithSalt` kubernetesUserDetails
      `Prelude.hashWithSalt` kubernetesWorkloadDetails

instance Prelude.NFData KubernetesDetails where
  rnf KubernetesDetails' {..} =
    Prelude.rnf kubernetesUserDetails
      `Prelude.seq` Prelude.rnf kubernetesWorkloadDetails
