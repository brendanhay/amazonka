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
-- Module      : Amazonka.Inspector2.Types.Cvss3
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Cvss3 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Common Vulnerability Scoring System (CVSS) version 3 details for the
-- vulnerability.
--
-- /See:/ 'newCvss3' smart constructor.
data Cvss3 = Cvss3'
  { -- | The CVSS v3 base score for the vulnerability.
    baseScore :: Prelude.Maybe Prelude.Double,
    -- | The scoring vector associated with the CVSS v3 score.
    scoringVector :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cvss3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseScore', 'cvss3_baseScore' - The CVSS v3 base score for the vulnerability.
--
-- 'scoringVector', 'cvss3_scoringVector' - The scoring vector associated with the CVSS v3 score.
newCvss3 ::
  Cvss3
newCvss3 =
  Cvss3'
    { baseScore = Prelude.Nothing,
      scoringVector = Prelude.Nothing
    }

-- | The CVSS v3 base score for the vulnerability.
cvss3_baseScore :: Lens.Lens' Cvss3 (Prelude.Maybe Prelude.Double)
cvss3_baseScore = Lens.lens (\Cvss3' {baseScore} -> baseScore) (\s@Cvss3' {} a -> s {baseScore = a} :: Cvss3)

-- | The scoring vector associated with the CVSS v3 score.
cvss3_scoringVector :: Lens.Lens' Cvss3 (Prelude.Maybe Prelude.Text)
cvss3_scoringVector = Lens.lens (\Cvss3' {scoringVector} -> scoringVector) (\s@Cvss3' {} a -> s {scoringVector = a} :: Cvss3)

instance Data.FromJSON Cvss3 where
  parseJSON =
    Data.withObject
      "Cvss3"
      ( \x ->
          Cvss3'
            Prelude.<$> (x Data..:? "baseScore")
            Prelude.<*> (x Data..:? "scoringVector")
      )

instance Prelude.Hashable Cvss3 where
  hashWithSalt _salt Cvss3' {..} =
    _salt
      `Prelude.hashWithSalt` baseScore
      `Prelude.hashWithSalt` scoringVector

instance Prelude.NFData Cvss3 where
  rnf Cvss3' {..} =
    Prelude.rnf baseScore
      `Prelude.seq` Prelude.rnf scoringVector
