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
-- Module      : Amazonka.Config.Types.AggregateConformancePackComplianceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackComplianceSummary where

import Amazonka.Config.Types.AggregateConformancePackComplianceCount
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of compliance based on either account ID or region.
--
-- /See:/ 'newAggregateConformancePackComplianceSummary' smart constructor.
data AggregateConformancePackComplianceSummary = AggregateConformancePackComplianceSummary'
  { -- | Returns an @AggregateConformancePackComplianceCount@ object.
    complianceSummary :: Prelude.Maybe AggregateConformancePackComplianceCount,
    -- | Groups the result based on Amazon Web Services account ID or Amazon Web
    -- Services Region.
    groupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateConformancePackComplianceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummary', 'aggregateConformancePackComplianceSummary_complianceSummary' - Returns an @AggregateConformancePackComplianceCount@ object.
--
-- 'groupName', 'aggregateConformancePackComplianceSummary_groupName' - Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
newAggregateConformancePackComplianceSummary ::
  AggregateConformancePackComplianceSummary
newAggregateConformancePackComplianceSummary =
  AggregateConformancePackComplianceSummary'
    { complianceSummary =
        Prelude.Nothing,
      groupName = Prelude.Nothing
    }

-- | Returns an @AggregateConformancePackComplianceCount@ object.
aggregateConformancePackComplianceSummary_complianceSummary :: Lens.Lens' AggregateConformancePackComplianceSummary (Prelude.Maybe AggregateConformancePackComplianceCount)
aggregateConformancePackComplianceSummary_complianceSummary = Lens.lens (\AggregateConformancePackComplianceSummary' {complianceSummary} -> complianceSummary) (\s@AggregateConformancePackComplianceSummary' {} a -> s {complianceSummary = a} :: AggregateConformancePackComplianceSummary)

-- | Groups the result based on Amazon Web Services account ID or Amazon Web
-- Services Region.
aggregateConformancePackComplianceSummary_groupName :: Lens.Lens' AggregateConformancePackComplianceSummary (Prelude.Maybe Prelude.Text)
aggregateConformancePackComplianceSummary_groupName = Lens.lens (\AggregateConformancePackComplianceSummary' {groupName} -> groupName) (\s@AggregateConformancePackComplianceSummary' {} a -> s {groupName = a} :: AggregateConformancePackComplianceSummary)

instance
  Data.FromJSON
    AggregateConformancePackComplianceSummary
  where
  parseJSON =
    Data.withObject
      "AggregateConformancePackComplianceSummary"
      ( \x ->
          AggregateConformancePackComplianceSummary'
            Prelude.<$> (x Data..:? "ComplianceSummary")
              Prelude.<*> (x Data..:? "GroupName")
      )

instance
  Prelude.Hashable
    AggregateConformancePackComplianceSummary
  where
  hashWithSalt
    _salt
    AggregateConformancePackComplianceSummary' {..} =
      _salt `Prelude.hashWithSalt` complianceSummary
        `Prelude.hashWithSalt` groupName

instance
  Prelude.NFData
    AggregateConformancePackComplianceSummary
  where
  rnf AggregateConformancePackComplianceSummary' {..} =
    Prelude.rnf complianceSummary
      `Prelude.seq` Prelude.rnf groupName
