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
-- Module      : Amazonka.ServiceCatalog.Types.ConstraintSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ConstraintSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a constraint.
--
-- /See:/ 'newConstraintSummary' smart constructor.
data ConstraintSummary = ConstraintSummary'
  { -- | The type of constraint.
    --
    -- -   @LAUNCH@
    --
    -- -   @NOTIFICATION@
    --
    -- -   STACKSET
    --
    -- -   @TEMPLATE@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The description of the constraint.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConstraintSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'description', 'constraintSummary_description' - The description of the constraint.
newConstraintSummary ::
  ConstraintSummary
newConstraintSummary =
  ConstraintSummary'
    { type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The type of constraint.
--
-- -   @LAUNCH@
--
-- -   @NOTIFICATION@
--
-- -   STACKSET
--
-- -   @TEMPLATE@
constraintSummary_type :: Lens.Lens' ConstraintSummary (Prelude.Maybe Prelude.Text)
constraintSummary_type = Lens.lens (\ConstraintSummary' {type'} -> type') (\s@ConstraintSummary' {} a -> s {type' = a} :: ConstraintSummary)

-- | The description of the constraint.
constraintSummary_description :: Lens.Lens' ConstraintSummary (Prelude.Maybe Prelude.Text)
constraintSummary_description = Lens.lens (\ConstraintSummary' {description} -> description) (\s@ConstraintSummary' {} a -> s {description = a} :: ConstraintSummary)

instance Data.FromJSON ConstraintSummary where
  parseJSON =
    Data.withObject
      "ConstraintSummary"
      ( \x ->
          ConstraintSummary'
            Prelude.<$> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable ConstraintSummary where
  hashWithSalt _salt ConstraintSummary' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` description

instance Prelude.NFData ConstraintSummary where
  rnf ConstraintSummary' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf description
