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
-- Module      : Amazonka.Inspector2.Types.EpssDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EpssDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the Exploit Prediction Scoring System (EPSS) score for a
-- finding.
--
-- /See:/ 'newEpssDetails' smart constructor.
data EpssDetails = EpssDetails'
  { -- | The EPSS score.
    score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EpssDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'score', 'epssDetails_score' - The EPSS score.
newEpssDetails ::
  EpssDetails
newEpssDetails =
  EpssDetails' {score = Prelude.Nothing}

-- | The EPSS score.
epssDetails_score :: Lens.Lens' EpssDetails (Prelude.Maybe Prelude.Double)
epssDetails_score = Lens.lens (\EpssDetails' {score} -> score) (\s@EpssDetails' {} a -> s {score = a} :: EpssDetails)

instance Data.FromJSON EpssDetails where
  parseJSON =
    Data.withObject
      "EpssDetails"
      ( \x ->
          EpssDetails' Prelude.<$> (x Data..:? "score")
      )

instance Prelude.Hashable EpssDetails where
  hashWithSalt _salt EpssDetails' {..} =
    _salt `Prelude.hashWithSalt` score

instance Prelude.NFData EpssDetails where
  rnf EpssDetails' {..} = Prelude.rnf score
