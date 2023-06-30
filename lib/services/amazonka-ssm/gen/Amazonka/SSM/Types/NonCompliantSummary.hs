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
-- Module      : Amazonka.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.NonCompliantSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.SeveritySummary

-- | A summary of resources that aren\'t compliant. The summary is organized
-- according to resource type.
--
-- /See:/ 'newNonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { -- | The total number of compliance items that aren\'t compliant.
    nonCompliantCount :: Prelude.Maybe Prelude.Int,
    -- | A summary of the non-compliance severity by compliance type
    severitySummary :: Prelude.Maybe SeveritySummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NonCompliantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nonCompliantCount', 'nonCompliantSummary_nonCompliantCount' - The total number of compliance items that aren\'t compliant.
--
-- 'severitySummary', 'nonCompliantSummary_severitySummary' - A summary of the non-compliance severity by compliance type
newNonCompliantSummary ::
  NonCompliantSummary
newNonCompliantSummary =
  NonCompliantSummary'
    { nonCompliantCount =
        Prelude.Nothing,
      severitySummary = Prelude.Nothing
    }

-- | The total number of compliance items that aren\'t compliant.
nonCompliantSummary_nonCompliantCount :: Lens.Lens' NonCompliantSummary (Prelude.Maybe Prelude.Int)
nonCompliantSummary_nonCompliantCount = Lens.lens (\NonCompliantSummary' {nonCompliantCount} -> nonCompliantCount) (\s@NonCompliantSummary' {} a -> s {nonCompliantCount = a} :: NonCompliantSummary)

-- | A summary of the non-compliance severity by compliance type
nonCompliantSummary_severitySummary :: Lens.Lens' NonCompliantSummary (Prelude.Maybe SeveritySummary)
nonCompliantSummary_severitySummary = Lens.lens (\NonCompliantSummary' {severitySummary} -> severitySummary) (\s@NonCompliantSummary' {} a -> s {severitySummary = a} :: NonCompliantSummary)

instance Data.FromJSON NonCompliantSummary where
  parseJSON =
    Data.withObject
      "NonCompliantSummary"
      ( \x ->
          NonCompliantSummary'
            Prelude.<$> (x Data..:? "NonCompliantCount")
            Prelude.<*> (x Data..:? "SeveritySummary")
      )

instance Prelude.Hashable NonCompliantSummary where
  hashWithSalt _salt NonCompliantSummary' {..} =
    _salt
      `Prelude.hashWithSalt` nonCompliantCount
      `Prelude.hashWithSalt` severitySummary

instance Prelude.NFData NonCompliantSummary where
  rnf NonCompliantSummary' {..} =
    Prelude.rnf nonCompliantCount
      `Prelude.seq` Prelude.rnf severitySummary
