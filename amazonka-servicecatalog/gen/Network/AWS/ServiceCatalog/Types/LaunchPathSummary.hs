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
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPathSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPathSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.Tag

-- | Summary information about a product path for a user.
--
-- /See:/ 'newLaunchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { -- | The constraints on the portfolio-product relationship.
    constraintSummaries :: Core.Maybe [ConstraintSummary],
    -- | The identifier of the product path.
    id :: Core.Maybe Core.Text,
    -- | The name of the portfolio to which the user was assigned.
    name :: Core.Maybe Core.Text,
    -- | The tags associated with this product path.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'name', 'launchPathSummary_name' - The name of the portfolio to which the user was assigned.
--
-- 'tags', 'launchPathSummary_tags' - The tags associated with this product path.
newLaunchPathSummary ::
  LaunchPathSummary
newLaunchPathSummary =
  LaunchPathSummary'
    { constraintSummaries =
        Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | The constraints on the portfolio-product relationship.
launchPathSummary_constraintSummaries :: Lens.Lens' LaunchPathSummary (Core.Maybe [ConstraintSummary])
launchPathSummary_constraintSummaries = Lens.lens (\LaunchPathSummary' {constraintSummaries} -> constraintSummaries) (\s@LaunchPathSummary' {} a -> s {constraintSummaries = a} :: LaunchPathSummary) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the product path.
launchPathSummary_id :: Lens.Lens' LaunchPathSummary (Core.Maybe Core.Text)
launchPathSummary_id = Lens.lens (\LaunchPathSummary' {id} -> id) (\s@LaunchPathSummary' {} a -> s {id = a} :: LaunchPathSummary)

-- | The name of the portfolio to which the user was assigned.
launchPathSummary_name :: Lens.Lens' LaunchPathSummary (Core.Maybe Core.Text)
launchPathSummary_name = Lens.lens (\LaunchPathSummary' {name} -> name) (\s@LaunchPathSummary' {} a -> s {name = a} :: LaunchPathSummary)

-- | The tags associated with this product path.
launchPathSummary_tags :: Lens.Lens' LaunchPathSummary (Core.Maybe [Tag])
launchPathSummary_tags = Lens.lens (\LaunchPathSummary' {tags} -> tags) (\s@LaunchPathSummary' {} a -> s {tags = a} :: LaunchPathSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON LaunchPathSummary where
  parseJSON =
    Core.withObject
      "LaunchPathSummary"
      ( \x ->
          LaunchPathSummary'
            Core.<$> ( x Core..:? "ConstraintSummaries"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Tags" Core..!= Core.mempty)
      )

instance Core.Hashable LaunchPathSummary

instance Core.NFData LaunchPathSummary
