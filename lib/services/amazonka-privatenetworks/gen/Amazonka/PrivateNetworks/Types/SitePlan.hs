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
-- Module      : Amazonka.PrivateNetworks.Types.SitePlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.SitePlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.NameValuePair
import Amazonka.PrivateNetworks.Types.NetworkResourceDefinition

-- | Information about a site plan.
--
-- /See:/ 'newSitePlan' smart constructor.
data SitePlan = SitePlan'
  { -- | The resource definitions of the plan.
    resourceDefinitions :: Prelude.Maybe [NetworkResourceDefinition],
    -- | The options of the plan.
    options :: Prelude.Maybe [NameValuePair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SitePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitions', 'sitePlan_resourceDefinitions' - The resource definitions of the plan.
--
-- 'options', 'sitePlan_options' - The options of the plan.
newSitePlan ::
  SitePlan
newSitePlan =
  SitePlan'
    { resourceDefinitions = Prelude.Nothing,
      options = Prelude.Nothing
    }

-- | The resource definitions of the plan.
sitePlan_resourceDefinitions :: Lens.Lens' SitePlan (Prelude.Maybe [NetworkResourceDefinition])
sitePlan_resourceDefinitions = Lens.lens (\SitePlan' {resourceDefinitions} -> resourceDefinitions) (\s@SitePlan' {} a -> s {resourceDefinitions = a} :: SitePlan) Prelude.. Lens.mapping Lens.coerced

-- | The options of the plan.
sitePlan_options :: Lens.Lens' SitePlan (Prelude.Maybe [NameValuePair])
sitePlan_options = Lens.lens (\SitePlan' {options} -> options) (\s@SitePlan' {} a -> s {options = a} :: SitePlan) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SitePlan where
  parseJSON =
    Core.withObject
      "SitePlan"
      ( \x ->
          SitePlan'
            Prelude.<$> ( x Core..:? "resourceDefinitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "options" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SitePlan where
  hashWithSalt _salt SitePlan' {..} =
    _salt `Prelude.hashWithSalt` resourceDefinitions
      `Prelude.hashWithSalt` options

instance Prelude.NFData SitePlan where
  rnf SitePlan' {..} =
    Prelude.rnf resourceDefinitions
      `Prelude.seq` Prelude.rnf options

instance Core.ToJSON SitePlan where
  toJSON SitePlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resourceDefinitions" Core..=)
              Prelude.<$> resourceDefinitions,
            ("options" Core..=) Prelude.<$> options
          ]
      )
