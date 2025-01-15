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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PartialMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The reference rule that partially matches the @ViolationTarget@ rule and
-- violation reason.
--
-- /See:/ 'newPartialMatch' smart constructor.
data PartialMatch = PartialMatch'
  { -- | The reference rule from the primary security group of the Firewall
    -- Manager policy.
    reference :: Prelude.Maybe Prelude.Text,
    -- | The violation reason.
    targetViolationReasons :: Prelude.Maybe [Prelude.Text]
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
-- 'reference', 'partialMatch_reference' - The reference rule from the primary security group of the Firewall
-- Manager policy.
--
-- 'targetViolationReasons', 'partialMatch_targetViolationReasons' - The violation reason.
newPartialMatch ::
  PartialMatch
newPartialMatch =
  PartialMatch'
    { reference = Prelude.Nothing,
      targetViolationReasons = Prelude.Nothing
    }

-- | The reference rule from the primary security group of the Firewall
-- Manager policy.
partialMatch_reference :: Lens.Lens' PartialMatch (Prelude.Maybe Prelude.Text)
partialMatch_reference = Lens.lens (\PartialMatch' {reference} -> reference) (\s@PartialMatch' {} a -> s {reference = a} :: PartialMatch)

-- | The violation reason.
partialMatch_targetViolationReasons :: Lens.Lens' PartialMatch (Prelude.Maybe [Prelude.Text])
partialMatch_targetViolationReasons = Lens.lens (\PartialMatch' {targetViolationReasons} -> targetViolationReasons) (\s@PartialMatch' {} a -> s {targetViolationReasons = a} :: PartialMatch) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PartialMatch where
  parseJSON =
    Data.withObject
      "PartialMatch"
      ( \x ->
          PartialMatch'
            Prelude.<$> (x Data..:? "Reference")
            Prelude.<*> ( x
                            Data..:? "TargetViolationReasons"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PartialMatch where
  hashWithSalt _salt PartialMatch' {..} =
    _salt
      `Prelude.hashWithSalt` reference
      `Prelude.hashWithSalt` targetViolationReasons

instance Prelude.NFData PartialMatch where
  rnf PartialMatch' {..} =
    Prelude.rnf reference `Prelude.seq`
      Prelude.rnf targetViolationReasons
