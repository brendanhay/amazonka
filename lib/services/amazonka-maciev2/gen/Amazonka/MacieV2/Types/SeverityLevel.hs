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
-- Module      : Amazonka.MacieV2.Types.SeverityLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SeverityLevel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.DataIdentifierSeverity
import qualified Amazonka.Prelude as Prelude

-- | Specifies a severity level for findings that a custom data identifier
-- produces. A severity level determines which severity is assigned to the
-- findings, based on the number of occurrences of text that matches the
-- custom data identifier\'s detection criteria.
--
-- /See:/ 'newSeverityLevel' smart constructor.
data SeverityLevel = SeverityLevel'
  { -- | The minimum number of occurrences of text that must match the custom
    -- data identifier\'s detection criteria in order to produce a finding with
    -- the specified severity (severity).
    occurrencesThreshold :: Prelude.Integer,
    -- | The severity to assign to a finding: if the number of occurrences is
    -- greater than or equal to the specified threshold (occurrencesThreshold);
    -- and, if applicable, the number of occurrences is less than the threshold
    -- for the next consecutive severity level for the custom data identifier,
    -- moving from LOW to HIGH.
    severity :: DataIdentifierSeverity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SeverityLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'occurrencesThreshold', 'severityLevel_occurrencesThreshold' - The minimum number of occurrences of text that must match the custom
-- data identifier\'s detection criteria in order to produce a finding with
-- the specified severity (severity).
--
-- 'severity', 'severityLevel_severity' - The severity to assign to a finding: if the number of occurrences is
-- greater than or equal to the specified threshold (occurrencesThreshold);
-- and, if applicable, the number of occurrences is less than the threshold
-- for the next consecutive severity level for the custom data identifier,
-- moving from LOW to HIGH.
newSeverityLevel ::
  -- | 'occurrencesThreshold'
  Prelude.Integer ->
  -- | 'severity'
  DataIdentifierSeverity ->
  SeverityLevel
newSeverityLevel pOccurrencesThreshold_ pSeverity_ =
  SeverityLevel'
    { occurrencesThreshold =
        pOccurrencesThreshold_,
      severity = pSeverity_
    }

-- | The minimum number of occurrences of text that must match the custom
-- data identifier\'s detection criteria in order to produce a finding with
-- the specified severity (severity).
severityLevel_occurrencesThreshold :: Lens.Lens' SeverityLevel Prelude.Integer
severityLevel_occurrencesThreshold = Lens.lens (\SeverityLevel' {occurrencesThreshold} -> occurrencesThreshold) (\s@SeverityLevel' {} a -> s {occurrencesThreshold = a} :: SeverityLevel)

-- | The severity to assign to a finding: if the number of occurrences is
-- greater than or equal to the specified threshold (occurrencesThreshold);
-- and, if applicable, the number of occurrences is less than the threshold
-- for the next consecutive severity level for the custom data identifier,
-- moving from LOW to HIGH.
severityLevel_severity :: Lens.Lens' SeverityLevel DataIdentifierSeverity
severityLevel_severity = Lens.lens (\SeverityLevel' {severity} -> severity) (\s@SeverityLevel' {} a -> s {severity = a} :: SeverityLevel)

instance Core.FromJSON SeverityLevel where
  parseJSON =
    Core.withObject
      "SeverityLevel"
      ( \x ->
          SeverityLevel'
            Prelude.<$> (x Core..: "occurrencesThreshold")
            Prelude.<*> (x Core..: "severity")
      )

instance Prelude.Hashable SeverityLevel where
  hashWithSalt _salt SeverityLevel' {..} =
    _salt `Prelude.hashWithSalt` occurrencesThreshold
      `Prelude.hashWithSalt` severity

instance Prelude.NFData SeverityLevel where
  rnf SeverityLevel' {..} =
    Prelude.rnf occurrencesThreshold
      `Prelude.seq` Prelude.rnf severity

instance Core.ToJSON SeverityLevel where
  toJSON SeverityLevel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "occurrencesThreshold"
                  Core..= occurrencesThreshold
              ),
            Prelude.Just ("severity" Core..= severity)
          ]
      )
