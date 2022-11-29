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
-- Module      : Amazonka.SecurityHub.Types.FindingProviderFields
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingProviderFields where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FindingProviderSeverity
import Amazonka.SecurityHub.Types.RelatedFinding

-- | In a @BatchImportFindings@ request, finding providers use
-- @FindingProviderFields@ to provide and update values for confidence,
-- criticality, related findings, severity, and types.
--
-- /See:/ 'newFindingProviderFields' smart constructor.
data FindingProviderFields = FindingProviderFields'
  { -- | The level of importance assigned to the resources associated with the
    -- finding.
    --
    -- A score of 0 means that the underlying resources have no criticality,
    -- and a score of 100 is reserved for the most critical resources.
    criticality :: Prelude.Maybe Prelude.Natural,
    -- | The severity of a finding.
    severity :: Prelude.Maybe FindingProviderSeverity,
    -- | A list of findings that are related to the current finding.
    relatedFindings :: Prelude.Maybe [RelatedFinding],
    -- | A finding\'s confidence. Confidence is defined as the likelihood that a
    -- finding accurately identifies the behavior or issue that it was intended
    -- to identify.
    --
    -- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
    -- zero percent confidence and 100 means 100 percent confidence.
    confidence :: Prelude.Maybe Prelude.Natural,
    -- | One or more finding types in the format of
    -- @namespace\/category\/classifier@ that classify a finding.
    --
    -- Valid namespace values are: Software and Configuration Checks | TTPs |
    -- Effects | Unusual Behaviors | Sensitive Data Identifications
    types :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingProviderFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'criticality', 'findingProviderFields_criticality' - The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
--
-- 'severity', 'findingProviderFields_severity' - The severity of a finding.
--
-- 'relatedFindings', 'findingProviderFields_relatedFindings' - A list of findings that are related to the current finding.
--
-- 'confidence', 'findingProviderFields_confidence' - A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
--
-- 'types', 'findingProviderFields_types' - One or more finding types in the format of
-- @namespace\/category\/classifier@ that classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
newFindingProviderFields ::
  FindingProviderFields
newFindingProviderFields =
  FindingProviderFields'
    { criticality =
        Prelude.Nothing,
      severity = Prelude.Nothing,
      relatedFindings = Prelude.Nothing,
      confidence = Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | The level of importance assigned to the resources associated with the
-- finding.
--
-- A score of 0 means that the underlying resources have no criticality,
-- and a score of 100 is reserved for the most critical resources.
findingProviderFields_criticality :: Lens.Lens' FindingProviderFields (Prelude.Maybe Prelude.Natural)
findingProviderFields_criticality = Lens.lens (\FindingProviderFields' {criticality} -> criticality) (\s@FindingProviderFields' {} a -> s {criticality = a} :: FindingProviderFields)

-- | The severity of a finding.
findingProviderFields_severity :: Lens.Lens' FindingProviderFields (Prelude.Maybe FindingProviderSeverity)
findingProviderFields_severity = Lens.lens (\FindingProviderFields' {severity} -> severity) (\s@FindingProviderFields' {} a -> s {severity = a} :: FindingProviderFields)

-- | A list of findings that are related to the current finding.
findingProviderFields_relatedFindings :: Lens.Lens' FindingProviderFields (Prelude.Maybe [RelatedFinding])
findingProviderFields_relatedFindings = Lens.lens (\FindingProviderFields' {relatedFindings} -> relatedFindings) (\s@FindingProviderFields' {} a -> s {relatedFindings = a} :: FindingProviderFields) Prelude.. Lens.mapping Lens.coerced

-- | A finding\'s confidence. Confidence is defined as the likelihood that a
-- finding accurately identifies the behavior or issue that it was intended
-- to identify.
--
-- Confidence is scored on a 0-100 basis using a ratio scale, where 0 means
-- zero percent confidence and 100 means 100 percent confidence.
findingProviderFields_confidence :: Lens.Lens' FindingProviderFields (Prelude.Maybe Prelude.Natural)
findingProviderFields_confidence = Lens.lens (\FindingProviderFields' {confidence} -> confidence) (\s@FindingProviderFields' {} a -> s {confidence = a} :: FindingProviderFields)

-- | One or more finding types in the format of
-- @namespace\/category\/classifier@ that classify a finding.
--
-- Valid namespace values are: Software and Configuration Checks | TTPs |
-- Effects | Unusual Behaviors | Sensitive Data Identifications
findingProviderFields_types :: Lens.Lens' FindingProviderFields (Prelude.Maybe [Prelude.Text])
findingProviderFields_types = Lens.lens (\FindingProviderFields' {types} -> types) (\s@FindingProviderFields' {} a -> s {types = a} :: FindingProviderFields) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FindingProviderFields where
  parseJSON =
    Core.withObject
      "FindingProviderFields"
      ( \x ->
          FindingProviderFields'
            Prelude.<$> (x Core..:? "Criticality")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> ( x Core..:? "RelatedFindings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Types" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FindingProviderFields where
  hashWithSalt _salt FindingProviderFields' {..} =
    _salt `Prelude.hashWithSalt` criticality
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` relatedFindings
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` types

instance Prelude.NFData FindingProviderFields where
  rnf FindingProviderFields' {..} =
    Prelude.rnf criticality
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf relatedFindings
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf types

instance Core.ToJSON FindingProviderFields where
  toJSON FindingProviderFields' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Criticality" Core..=) Prelude.<$> criticality,
            ("Severity" Core..=) Prelude.<$> severity,
            ("RelatedFindings" Core..=)
              Prelude.<$> relatedFindings,
            ("Confidence" Core..=) Prelude.<$> confidence,
            ("Types" Core..=) Prelude.<$> types
          ]
      )
