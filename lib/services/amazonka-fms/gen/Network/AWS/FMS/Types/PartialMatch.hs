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
-- Module      : Amazonka.FMS.Types.PartialMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PartialMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The reference rule that partially matches the @ViolationTarget@ rule and
-- violation reason.
--
-- /See:/ 'newPartialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { -- | The violation reason.
    targetViolationReasons :: Prelude.Maybe [Prelude.Text],
    -- | The reference rule from the primary security group of the Firewall
    -- Manager policy.
    reference :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartialMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetViolationReasons', 'partialMatch_targetViolationReasons' - The violation reason.
--
-- 'reference', 'partialMatch_reference' - The reference rule from the primary security group of the Firewall
-- Manager policy.
newPartialMatch ::
  PartialMatch
newPartialMatch =
  PartialMatch'
    { targetViolationReasons =
        Prelude.Nothing,
      reference = Prelude.Nothing
    }

-- | The violation reason.
partialMatch_targetViolationReasons :: Lens.Lens' PartialMatch (Prelude.Maybe [Prelude.Text])
partialMatch_targetViolationReasons = Lens.lens (\PartialMatch' {targetViolationReasons} -> targetViolationReasons) (\s@PartialMatch' {} a -> s {targetViolationReasons = a} :: PartialMatch) Prelude.. Lens.mapping Lens.coerced

-- | The reference rule from the primary security group of the Firewall
-- Manager policy.
partialMatch_reference :: Lens.Lens' PartialMatch (Prelude.Maybe Prelude.Text)
partialMatch_reference = Lens.lens (\PartialMatch' {reference} -> reference) (\s@PartialMatch' {} a -> s {reference = a} :: PartialMatch)

instance Core.FromJSON PartialMatch where
  parseJSON =
    Core.withObject
      "PartialMatch"
      ( \x ->
          PartialMatch'
            Prelude.<$> ( x Core..:? "TargetViolationReasons"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Reference")
      )

instance Prelude.Hashable PartialMatch

instance Prelude.NFData PartialMatch
