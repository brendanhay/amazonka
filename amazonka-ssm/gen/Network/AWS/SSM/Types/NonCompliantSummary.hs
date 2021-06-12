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
-- Module      : Network.AWS.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NonCompliantSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are not compliant. The summary is organized
-- according to resource type.
--
-- /See:/ 'newNonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { -- | A summary of the non-compliance severity by compliance type
    severitySummary :: Core.Maybe SeveritySummary,
    -- | The total number of compliance items that are not compliant.
    nonCompliantCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NonCompliantSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severitySummary', 'nonCompliantSummary_severitySummary' - A summary of the non-compliance severity by compliance type
--
-- 'nonCompliantCount', 'nonCompliantSummary_nonCompliantCount' - The total number of compliance items that are not compliant.
newNonCompliantSummary ::
  NonCompliantSummary
newNonCompliantSummary =
  NonCompliantSummary'
    { severitySummary =
        Core.Nothing,
      nonCompliantCount = Core.Nothing
    }

-- | A summary of the non-compliance severity by compliance type
nonCompliantSummary_severitySummary :: Lens.Lens' NonCompliantSummary (Core.Maybe SeveritySummary)
nonCompliantSummary_severitySummary = Lens.lens (\NonCompliantSummary' {severitySummary} -> severitySummary) (\s@NonCompliantSummary' {} a -> s {severitySummary = a} :: NonCompliantSummary)

-- | The total number of compliance items that are not compliant.
nonCompliantSummary_nonCompliantCount :: Lens.Lens' NonCompliantSummary (Core.Maybe Core.Int)
nonCompliantSummary_nonCompliantCount = Lens.lens (\NonCompliantSummary' {nonCompliantCount} -> nonCompliantCount) (\s@NonCompliantSummary' {} a -> s {nonCompliantCount = a} :: NonCompliantSummary)

instance Core.FromJSON NonCompliantSummary where
  parseJSON =
    Core.withObject
      "NonCompliantSummary"
      ( \x ->
          NonCompliantSummary'
            Core.<$> (x Core..:? "SeveritySummary")
            Core.<*> (x Core..:? "NonCompliantCount")
      )

instance Core.Hashable NonCompliantSummary

instance Core.NFData NonCompliantSummary
