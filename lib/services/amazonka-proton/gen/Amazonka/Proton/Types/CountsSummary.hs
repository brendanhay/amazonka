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
-- Module      : Amazonka.Proton.Types.CountsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.CountsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.ResourceCountsSummary

-- | Summary counts of each Proton resource type.
--
-- /See:/ 'newCountsSummary' smart constructor.
data CountsSummary = CountsSummary'
  { -- | The total number of components in the Amazon Web Services account.
    --
    -- The semantics of the @components@ field are different from the semantics
    -- of results for other infrastructure-provisioning resources. That\'s
    -- because at this time components don\'t have associated templates,
    -- therefore they don\'t have the concept of staleness. The @components@
    -- object will only contain @total@ and @failed@ members.
    components :: Prelude.Maybe ResourceCountsSummary,
    -- | The total number of environment templates in the Amazon Web Services
    -- account. The @environmentTemplates@ object will only contain @total@
    -- members.
    environmentTemplates :: Prelude.Maybe ResourceCountsSummary,
    -- | The staleness counts for Proton environments in the Amazon Web Services
    -- account. The @environments@ object will only contain @total@ members.
    environments :: Prelude.Maybe ResourceCountsSummary,
    -- | The staleness counts for Proton pipelines in the Amazon Web Services
    -- account.
    pipelines :: Prelude.Maybe ResourceCountsSummary,
    -- | The staleness counts for Proton service instances in the Amazon Web
    -- Services account.
    serviceInstances :: Prelude.Maybe ResourceCountsSummary,
    -- | The total number of service templates in the Amazon Web Services
    -- account. The @serviceTemplates@ object will only contain @total@
    -- members.
    serviceTemplates :: Prelude.Maybe ResourceCountsSummary,
    -- | The staleness counts for Proton services in the Amazon Web Services
    -- account.
    services :: Prelude.Maybe ResourceCountsSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CountsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'components', 'countsSummary_components' - The total number of components in the Amazon Web Services account.
--
-- The semantics of the @components@ field are different from the semantics
-- of results for other infrastructure-provisioning resources. That\'s
-- because at this time components don\'t have associated templates,
-- therefore they don\'t have the concept of staleness. The @components@
-- object will only contain @total@ and @failed@ members.
--
-- 'environmentTemplates', 'countsSummary_environmentTemplates' - The total number of environment templates in the Amazon Web Services
-- account. The @environmentTemplates@ object will only contain @total@
-- members.
--
-- 'environments', 'countsSummary_environments' - The staleness counts for Proton environments in the Amazon Web Services
-- account. The @environments@ object will only contain @total@ members.
--
-- 'pipelines', 'countsSummary_pipelines' - The staleness counts for Proton pipelines in the Amazon Web Services
-- account.
--
-- 'serviceInstances', 'countsSummary_serviceInstances' - The staleness counts for Proton service instances in the Amazon Web
-- Services account.
--
-- 'serviceTemplates', 'countsSummary_serviceTemplates' - The total number of service templates in the Amazon Web Services
-- account. The @serviceTemplates@ object will only contain @total@
-- members.
--
-- 'services', 'countsSummary_services' - The staleness counts for Proton services in the Amazon Web Services
-- account.
newCountsSummary ::
  CountsSummary
newCountsSummary =
  CountsSummary'
    { components = Prelude.Nothing,
      environmentTemplates = Prelude.Nothing,
      environments = Prelude.Nothing,
      pipelines = Prelude.Nothing,
      serviceInstances = Prelude.Nothing,
      serviceTemplates = Prelude.Nothing,
      services = Prelude.Nothing
    }

-- | The total number of components in the Amazon Web Services account.
--
-- The semantics of the @components@ field are different from the semantics
-- of results for other infrastructure-provisioning resources. That\'s
-- because at this time components don\'t have associated templates,
-- therefore they don\'t have the concept of staleness. The @components@
-- object will only contain @total@ and @failed@ members.
countsSummary_components :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_components = Lens.lens (\CountsSummary' {components} -> components) (\s@CountsSummary' {} a -> s {components = a} :: CountsSummary)

-- | The total number of environment templates in the Amazon Web Services
-- account. The @environmentTemplates@ object will only contain @total@
-- members.
countsSummary_environmentTemplates :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_environmentTemplates = Lens.lens (\CountsSummary' {environmentTemplates} -> environmentTemplates) (\s@CountsSummary' {} a -> s {environmentTemplates = a} :: CountsSummary)

-- | The staleness counts for Proton environments in the Amazon Web Services
-- account. The @environments@ object will only contain @total@ members.
countsSummary_environments :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_environments = Lens.lens (\CountsSummary' {environments} -> environments) (\s@CountsSummary' {} a -> s {environments = a} :: CountsSummary)

-- | The staleness counts for Proton pipelines in the Amazon Web Services
-- account.
countsSummary_pipelines :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_pipelines = Lens.lens (\CountsSummary' {pipelines} -> pipelines) (\s@CountsSummary' {} a -> s {pipelines = a} :: CountsSummary)

-- | The staleness counts for Proton service instances in the Amazon Web
-- Services account.
countsSummary_serviceInstances :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_serviceInstances = Lens.lens (\CountsSummary' {serviceInstances} -> serviceInstances) (\s@CountsSummary' {} a -> s {serviceInstances = a} :: CountsSummary)

-- | The total number of service templates in the Amazon Web Services
-- account. The @serviceTemplates@ object will only contain @total@
-- members.
countsSummary_serviceTemplates :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_serviceTemplates = Lens.lens (\CountsSummary' {serviceTemplates} -> serviceTemplates) (\s@CountsSummary' {} a -> s {serviceTemplates = a} :: CountsSummary)

-- | The staleness counts for Proton services in the Amazon Web Services
-- account.
countsSummary_services :: Lens.Lens' CountsSummary (Prelude.Maybe ResourceCountsSummary)
countsSummary_services = Lens.lens (\CountsSummary' {services} -> services) (\s@CountsSummary' {} a -> s {services = a} :: CountsSummary)

instance Data.FromJSON CountsSummary where
  parseJSON =
    Data.withObject
      "CountsSummary"
      ( \x ->
          CountsSummary'
            Prelude.<$> (x Data..:? "components")
            Prelude.<*> (x Data..:? "environmentTemplates")
            Prelude.<*> (x Data..:? "environments")
            Prelude.<*> (x Data..:? "pipelines")
            Prelude.<*> (x Data..:? "serviceInstances")
            Prelude.<*> (x Data..:? "serviceTemplates")
            Prelude.<*> (x Data..:? "services")
      )

instance Prelude.Hashable CountsSummary where
  hashWithSalt _salt CountsSummary' {..} =
    _salt
      `Prelude.hashWithSalt` components
      `Prelude.hashWithSalt` environmentTemplates
      `Prelude.hashWithSalt` environments
      `Prelude.hashWithSalt` pipelines
      `Prelude.hashWithSalt` serviceInstances
      `Prelude.hashWithSalt` serviceTemplates
      `Prelude.hashWithSalt` services

instance Prelude.NFData CountsSummary where
  rnf CountsSummary' {..} =
    Prelude.rnf components
      `Prelude.seq` Prelude.rnf environmentTemplates
      `Prelude.seq` Prelude.rnf environments
      `Prelude.seq` Prelude.rnf pipelines
      `Prelude.seq` Prelude.rnf serviceInstances
      `Prelude.seq` Prelude.rnf serviceTemplates
      `Prelude.seq` Prelude.rnf services
