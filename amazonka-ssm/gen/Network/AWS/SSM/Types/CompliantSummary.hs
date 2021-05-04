{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.Types.CompliantSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CompliantSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are compliant. The summary is organized
-- according to the resource count for each compliance type.
--
-- /See:/ 'newCompliantSummary' smart constructor.
data CompliantSummary = CompliantSummary'
  { -- | A summary of the compliance severity by compliance type.
    severitySummary :: Prelude.Maybe SeveritySummary,
    -- | The total number of resources that are compliant.
    compliantCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CompliantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severitySummary', 'compliantSummary_severitySummary' - A summary of the compliance severity by compliance type.
--
-- 'compliantCount', 'compliantSummary_compliantCount' - The total number of resources that are compliant.
newCompliantSummary ::
  CompliantSummary
newCompliantSummary =
  CompliantSummary'
    { severitySummary =
        Prelude.Nothing,
      compliantCount = Prelude.Nothing
    }

-- | A summary of the compliance severity by compliance type.
compliantSummary_severitySummary :: Lens.Lens' CompliantSummary (Prelude.Maybe SeveritySummary)
compliantSummary_severitySummary = Lens.lens (\CompliantSummary' {severitySummary} -> severitySummary) (\s@CompliantSummary' {} a -> s {severitySummary = a} :: CompliantSummary)

-- | The total number of resources that are compliant.
compliantSummary_compliantCount :: Lens.Lens' CompliantSummary (Prelude.Maybe Prelude.Int)
compliantSummary_compliantCount = Lens.lens (\CompliantSummary' {compliantCount} -> compliantCount) (\s@CompliantSummary' {} a -> s {compliantCount = a} :: CompliantSummary)

instance Prelude.FromJSON CompliantSummary where
  parseJSON =
    Prelude.withObject
      "CompliantSummary"
      ( \x ->
          CompliantSummary'
            Prelude.<$> (x Prelude..:? "SeveritySummary")
            Prelude.<*> (x Prelude..:? "CompliantCount")
      )

instance Prelude.Hashable CompliantSummary

instance Prelude.NFData CompliantSummary
