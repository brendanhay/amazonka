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
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ConstraintSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary information about a constraint.
--
-- /See:/ 'newConstraintSummary' smart constructor.
data ConstraintSummary = ConstraintSummary'
  { -- | The description of the constraint.
    description :: Core.Maybe Core.Text,
    -- | The type of constraint.
    --
    -- -   @LAUNCH@
    --
    -- -   @NOTIFICATION@
    --
    -- -   STACKSET
    --
    -- -   @TEMPLATE@
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConstraintSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'constraintSummary_description' - The description of the constraint.
--
-- 'type'', 'constraintSummary_type' - The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
newConstraintSummary ::
  ConstraintSummary
newConstraintSummary =
  ConstraintSummary'
    { description = Core.Nothing,
      type' = Core.Nothing
    }

-- | The description of the constraint.
constraintSummary_description :: Lens.Lens' ConstraintSummary (Core.Maybe Core.Text)
constraintSummary_description = Lens.lens (\ConstraintSummary' {description} -> description) (\s@ConstraintSummary' {} a -> s {description = a} :: ConstraintSummary)

-- | The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
constraintSummary_type :: Lens.Lens' ConstraintSummary (Core.Maybe Core.Text)
constraintSummary_type = Lens.lens (\ConstraintSummary' {type'} -> type') (\s@ConstraintSummary' {} a -> s {type' = a} :: ConstraintSummary)

instance Core.FromJSON ConstraintSummary where
  parseJSON =
    Core.withObject
      "ConstraintSummary"
      ( \x ->
          ConstraintSummary'
            Core.<$> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable ConstraintSummary

instance Core.NFData ConstraintSummary
