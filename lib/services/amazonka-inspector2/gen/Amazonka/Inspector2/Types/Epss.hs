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
-- Module      : Amazonka.Inspector2.Types.Epss
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Epss where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the Exploit Prediction Scoring System (EPSS) score.
--
-- /See:/ 'newEpss' smart constructor.
data Epss = Epss'
  { -- | The Exploit Prediction Scoring System (EPSS) score.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Epss' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'epss_score' - The Exploit Prediction Scoring System (EPSS) score.
newEpss ::
  Epss
newEpss = Epss' {score = Prelude.Nothing}

-- | The Exploit Prediction Scoring System (EPSS) score.
epss_score :: Lens.Lens' Epss (Prelude.Maybe Prelude.Double)
epss_score = Lens.lens (\Epss' {score} -> score) (\s@Epss' {} a -> s {score = a} :: Epss)

instance Data.FromJSON Epss where
  parseJSON =
    Data.withObject
      "Epss"
      (\x -> Epss' Prelude.<$> (x Data..:? "score"))

instance Prelude.Hashable Epss where
  hashWithSalt _salt Epss' {..} =
    _salt `Prelude.hashWithSalt` score

instance Prelude.NFData Epss where
  rnf Epss' {..} = Prelude.rnf score
