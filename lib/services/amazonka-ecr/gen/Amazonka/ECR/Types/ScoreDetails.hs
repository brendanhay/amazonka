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
-- Module      : Amazonka.ECR.Types.ScoreDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ScoreDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types.CvssScoreDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the Amazon Inspector score given to a finding.
--
-- /See:/ 'newScoreDetails' smart constructor.
data ScoreDetails = ScoreDetails'
  { -- | An object that contains details about the CVSS score given to a finding.
    cvss :: Prelude.Maybe CvssScoreDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScoreDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cvss', 'scoreDetails_cvss' - An object that contains details about the CVSS score given to a finding.
newScoreDetails ::
  ScoreDetails
newScoreDetails =
  ScoreDetails' {cvss = Prelude.Nothing}

-- | An object that contains details about the CVSS score given to a finding.
scoreDetails_cvss :: Lens.Lens' ScoreDetails (Prelude.Maybe CvssScoreDetails)
scoreDetails_cvss = Lens.lens (\ScoreDetails' {cvss} -> cvss) (\s@ScoreDetails' {} a -> s {cvss = a} :: ScoreDetails)

instance Data.FromJSON ScoreDetails where
  parseJSON =
    Data.withObject
      "ScoreDetails"
      ( \x ->
          ScoreDetails' Prelude.<$> (x Data..:? "cvss")
      )

instance Prelude.Hashable ScoreDetails where
  hashWithSalt _salt ScoreDetails' {..} =
    _salt `Prelude.hashWithSalt` cvss

instance Prelude.NFData ScoreDetails where
  rnf ScoreDetails' {..} = Prelude.rnf cvss
