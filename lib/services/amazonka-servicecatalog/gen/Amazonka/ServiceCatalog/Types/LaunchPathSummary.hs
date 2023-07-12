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
-- Module      : Amazonka.ServiceCatalog.Types.LaunchPathSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.LaunchPathSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ConstraintSummary
import Amazonka.ServiceCatalog.Types.Tag

-- | Summary information about a product path for a user.
--
-- /See:/ 'newLaunchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { -- | The constraints on the portfolio-product relationship.
    constraintSummaries :: Prelude.Maybe [ConstraintSummary],
    -- | The identifier of the product path.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the portfolio that contains the product.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with this product path.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchPathSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'constraintSummaries', 'launchPathSummary_constraintSummaries' - The constraints on the portfolio-product relationship.
--
-- 'id', 'launchPathSummary_id' - The identifier of the product path.
--
-- 'name', 'launchPathSummary_name' - The name of the portfolio that contains the product.
--
-- 'tags', 'launchPathSummary_tags' - The tags associated with this product path.
newLaunchPathSummary ::
  LaunchPathSummary
newLaunchPathSummary =
  LaunchPathSummary'
    { constraintSummaries =
        Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The constraints on the portfolio-product relationship.
launchPathSummary_constraintSummaries :: Lens.Lens' LaunchPathSummary (Prelude.Maybe [ConstraintSummary])
launchPathSummary_constraintSummaries = Lens.lens (\LaunchPathSummary' {constraintSummaries} -> constraintSummaries) (\s@LaunchPathSummary' {} a -> s {constraintSummaries = a} :: LaunchPathSummary) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the product path.
launchPathSummary_id :: Lens.Lens' LaunchPathSummary (Prelude.Maybe Prelude.Text)
launchPathSummary_id = Lens.lens (\LaunchPathSummary' {id} -> id) (\s@LaunchPathSummary' {} a -> s {id = a} :: LaunchPathSummary)

-- | The name of the portfolio that contains the product.
launchPathSummary_name :: Lens.Lens' LaunchPathSummary (Prelude.Maybe Prelude.Text)
launchPathSummary_name = Lens.lens (\LaunchPathSummary' {name} -> name) (\s@LaunchPathSummary' {} a -> s {name = a} :: LaunchPathSummary)

-- | The tags associated with this product path.
launchPathSummary_tags :: Lens.Lens' LaunchPathSummary (Prelude.Maybe [Tag])
launchPathSummary_tags = Lens.lens (\LaunchPathSummary' {tags} -> tags) (\s@LaunchPathSummary' {} a -> s {tags = a} :: LaunchPathSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LaunchPathSummary where
  parseJSON =
    Data.withObject
      "LaunchPathSummary"
      ( \x ->
          LaunchPathSummary'
            Prelude.<$> ( x
                            Data..:? "ConstraintSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable LaunchPathSummary where
  hashWithSalt _salt LaunchPathSummary' {..} =
    _salt
      `Prelude.hashWithSalt` constraintSummaries
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags

instance Prelude.NFData LaunchPathSummary where
  rnf LaunchPathSummary' {..} =
    Prelude.rnf constraintSummaries
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
