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
-- Module      : Amazonka.Inspector2.Types.Cvss2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Cvss2 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Common Vulnerability Scoring System (CVSS) version 2 details for the
-- vulnerability.
--
-- /See:/ 'newCvss2' smart constructor.
data Cvss2 = Cvss2'
  { -- | The CVSS v2 base score for the vulnerability.
    baseScore :: Prelude.Maybe Prelude.Double,
    -- | The scoring vector associated with the CVSS v2 score.
    scoringVector :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cvss2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseScore', 'cvss2_baseScore' - The CVSS v2 base score for the vulnerability.
--
-- 'scoringVector', 'cvss2_scoringVector' - The scoring vector associated with the CVSS v2 score.
newCvss2 ::
  Cvss2
newCvss2 =
  Cvss2'
    { baseScore = Prelude.Nothing,
      scoringVector = Prelude.Nothing
    }

-- | The CVSS v2 base score for the vulnerability.
cvss2_baseScore :: Lens.Lens' Cvss2 (Prelude.Maybe Prelude.Double)
cvss2_baseScore = Lens.lens (\Cvss2' {baseScore} -> baseScore) (\s@Cvss2' {} a -> s {baseScore = a} :: Cvss2)

-- | The scoring vector associated with the CVSS v2 score.
cvss2_scoringVector :: Lens.Lens' Cvss2 (Prelude.Maybe Prelude.Text)
cvss2_scoringVector = Lens.lens (\Cvss2' {scoringVector} -> scoringVector) (\s@Cvss2' {} a -> s {scoringVector = a} :: Cvss2)

instance Data.FromJSON Cvss2 where
  parseJSON =
    Data.withObject
      "Cvss2"
      ( \x ->
          Cvss2'
            Prelude.<$> (x Data..:? "baseScore")
            Prelude.<*> (x Data..:? "scoringVector")
      )

instance Prelude.Hashable Cvss2 where
  hashWithSalt _salt Cvss2' {..} =
    _salt
      `Prelude.hashWithSalt` baseScore
      `Prelude.hashWithSalt` scoringVector

instance Prelude.NFData Cvss2 where
  rnf Cvss2' {..} =
    Prelude.rnf baseScore
      `Prelude.seq` Prelude.rnf scoringVector
