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
-- Module      : Amazonka.MacieV2.Types.Severity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Severity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SeverityDescription
import qualified Amazonka.Prelude as Prelude

-- | Provides the numerical and qualitative representations of a finding\'s
-- severity.
--
-- /See:/ 'newSeverity' smart constructor.
data Severity = Severity'
  { -- | The qualitative representation of the finding\'s severity, ranging from
    -- Low (least severe) to High (most severe).
    description :: Prelude.Maybe SeverityDescription,
    -- | The numerical representation of the finding\'s severity, ranging from 1
    -- (least severe) to 3 (most severe).
    score :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Severity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'severity_description' - The qualitative representation of the finding\'s severity, ranging from
-- Low (least severe) to High (most severe).
--
-- 'score', 'severity_score' - The numerical representation of the finding\'s severity, ranging from 1
-- (least severe) to 3 (most severe).
newSeverity ::
  Severity
newSeverity =
  Severity'
    { description = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The qualitative representation of the finding\'s severity, ranging from
-- Low (least severe) to High (most severe).
severity_description :: Lens.Lens' Severity (Prelude.Maybe SeverityDescription)
severity_description = Lens.lens (\Severity' {description} -> description) (\s@Severity' {} a -> s {description = a} :: Severity)

-- | The numerical representation of the finding\'s severity, ranging from 1
-- (least severe) to 3 (most severe).
severity_score :: Lens.Lens' Severity (Prelude.Maybe Prelude.Integer)
severity_score = Lens.lens (\Severity' {score} -> score) (\s@Severity' {} a -> s {score = a} :: Severity)

instance Data.FromJSON Severity where
  parseJSON =
    Data.withObject
      "Severity"
      ( \x ->
          Severity'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "score")
      )

instance Prelude.Hashable Severity where
  hashWithSalt _salt Severity' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` score

instance Prelude.NFData Severity where
  rnf Severity' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf score
