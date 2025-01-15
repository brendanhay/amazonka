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
-- Module      : Amazonka.SSM.Types.CompliantSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.CompliantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.SeveritySummary

-- | A summary of resources that are compliant. The summary is organized
-- according to the resource count for each compliance type.
--
-- /See:/ 'newCompliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { -- | The total number of resources that are compliant.
    compliantCount :: Prelude.Maybe Prelude.Int,
    -- | A summary of the compliance severity by compliance type.
    severitySummary :: Prelude.Maybe SeveritySummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompliantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliantCount', 'compliantSummary_compliantCount' - The total number of resources that are compliant.
--
-- 'severitySummary', 'compliantSummary_severitySummary' - A summary of the compliance severity by compliance type.
newCompliantSummary ::
  CompliantSummary
newCompliantSummary =
  CompliantSummary'
    { compliantCount = Prelude.Nothing,
      severitySummary = Prelude.Nothing
    }

-- | The total number of resources that are compliant.
compliantSummary_compliantCount :: Lens.Lens' CompliantSummary (Prelude.Maybe Prelude.Int)
compliantSummary_compliantCount = Lens.lens (\CompliantSummary' {compliantCount} -> compliantCount) (\s@CompliantSummary' {} a -> s {compliantCount = a} :: CompliantSummary)

-- | A summary of the compliance severity by compliance type.
compliantSummary_severitySummary :: Lens.Lens' CompliantSummary (Prelude.Maybe SeveritySummary)
compliantSummary_severitySummary = Lens.lens (\CompliantSummary' {severitySummary} -> severitySummary) (\s@CompliantSummary' {} a -> s {severitySummary = a} :: CompliantSummary)

instance Data.FromJSON CompliantSummary where
  parseJSON =
    Data.withObject
      "CompliantSummary"
      ( \x ->
          CompliantSummary'
            Prelude.<$> (x Data..:? "CompliantCount")
            Prelude.<*> (x Data..:? "SeveritySummary")
      )

instance Prelude.Hashable CompliantSummary where
  hashWithSalt _salt CompliantSummary' {..} =
    _salt
      `Prelude.hashWithSalt` compliantCount
      `Prelude.hashWithSalt` severitySummary

instance Prelude.NFData CompliantSummary where
  rnf CompliantSummary' {..} =
    Prelude.rnf compliantCount `Prelude.seq`
      Prelude.rnf severitySummary
