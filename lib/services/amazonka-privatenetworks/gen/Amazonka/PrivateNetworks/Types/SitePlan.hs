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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.SitePlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types.NameValuePair
import Amazonka.PrivateNetworks.Types.NetworkResourceDefinition

-- | Information about a site plan.
--
-- /See:/ 'newSitePlan' smart constructor.
data SitePlan = SitePlan'
  { -- | The options of the plan.
    options :: Prelude.Maybe [NameValuePair],
    -- | The resource definitions of the plan.
    resourceDefinitions :: Prelude.Maybe [NetworkResourceDefinition]
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
-- 'options', 'sitePlan_options' - The options of the plan.
--
-- 'resourceDefinitions', 'sitePlan_resourceDefinitions' - The resource definitions of the plan.
newSitePlan ::
  SitePlan
newSitePlan =
  SitePlan'
    { options = Prelude.Nothing,
      resourceDefinitions = Prelude.Nothing
    }

-- | The options of the plan.
sitePlan_options :: Lens.Lens' SitePlan (Prelude.Maybe [NameValuePair])
sitePlan_options = Lens.lens (\SitePlan' {options} -> options) (\s@SitePlan' {} a -> s {options = a} :: SitePlan) Prelude.. Lens.mapping Lens.coerced

-- | The resource definitions of the plan.
sitePlan_resourceDefinitions :: Lens.Lens' SitePlan (Prelude.Maybe [NetworkResourceDefinition])
sitePlan_resourceDefinitions = Lens.lens (\SitePlan' {resourceDefinitions} -> resourceDefinitions) (\s@SitePlan' {} a -> s {resourceDefinitions = a} :: SitePlan) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SitePlan where
  parseJSON =
    Data.withObject
      "SitePlan"
      ( \x ->
          SitePlan'
            Prelude.<$> (x Data..:? "options" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "resourceDefinitions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SitePlan where
  hashWithSalt _salt SitePlan' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` resourceDefinitions

instance Prelude.NFData SitePlan where
  rnf SitePlan' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf resourceDefinitions

instance Data.ToJSON SitePlan where
  toJSON SitePlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("options" Data..=) Prelude.<$> options,
            ("resourceDefinitions" Data..=)
              Prelude.<$> resourceDefinitions
          ]
      )
