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
-- Module      : Network.AWS.SSM.Types.NonCompliantSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NonCompliantSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.SeveritySummary

-- | A summary of resources that are not compliant. The summary is organized
-- according to resource type.
--
-- /See:/ 'newNonCompliantSummary' smart constructor.
data NonCompliantSummary = NonCompliantSummary'
  { -- | A summary of the non-compliance severity by compliance type
    severitySummary :: Prelude.Maybe SeveritySummary,
    -- | The total number of compliance items that are not compliant.
    nonCompliantCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      nonCompliantCount = Prelude.Nothing
    }

-- | A summary of the non-compliance severity by compliance type
nonCompliantSummary_severitySummary :: Lens.Lens' NonCompliantSummary (Prelude.Maybe SeveritySummary)
nonCompliantSummary_severitySummary = Lens.lens (\NonCompliantSummary' {severitySummary} -> severitySummary) (\s@NonCompliantSummary' {} a -> s {severitySummary = a} :: NonCompliantSummary)

-- | The total number of compliance items that are not compliant.
nonCompliantSummary_nonCompliantCount :: Lens.Lens' NonCompliantSummary (Prelude.Maybe Prelude.Int)
nonCompliantSummary_nonCompliantCount = Lens.lens (\NonCompliantSummary' {nonCompliantCount} -> nonCompliantCount) (\s@NonCompliantSummary' {} a -> s {nonCompliantCount = a} :: NonCompliantSummary)

instance Prelude.FromJSON NonCompliantSummary where
  parseJSON =
    Prelude.withObject
      "NonCompliantSummary"
      ( \x ->
          NonCompliantSummary'
            Prelude.<$> (x Prelude..:? "SeveritySummary")
            Prelude.<*> (x Prelude..:? "NonCompliantCount")
      )

instance Prelude.Hashable NonCompliantSummary

instance Prelude.NFData NonCompliantSummary
