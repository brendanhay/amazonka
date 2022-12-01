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
-- Module      : Amazonka.Athena.Types.ResultReuseInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultReuseInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about whether the result of a previous query was
-- reused.
--
-- /See:/ 'newResultReuseInformation' smart constructor.
data ResultReuseInformation = ResultReuseInformation'
  { -- | True if a previous query result was reused; false if the result was
    -- generated from a new run of the query.
    reusedPreviousResult :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultReuseInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reusedPreviousResult', 'resultReuseInformation_reusedPreviousResult' - True if a previous query result was reused; false if the result was
-- generated from a new run of the query.
newResultReuseInformation ::
  -- | 'reusedPreviousResult'
  Prelude.Bool ->
  ResultReuseInformation
newResultReuseInformation pReusedPreviousResult_ =
  ResultReuseInformation'
    { reusedPreviousResult =
        pReusedPreviousResult_
    }

-- | True if a previous query result was reused; false if the result was
-- generated from a new run of the query.
resultReuseInformation_reusedPreviousResult :: Lens.Lens' ResultReuseInformation Prelude.Bool
resultReuseInformation_reusedPreviousResult = Lens.lens (\ResultReuseInformation' {reusedPreviousResult} -> reusedPreviousResult) (\s@ResultReuseInformation' {} a -> s {reusedPreviousResult = a} :: ResultReuseInformation)

instance Core.FromJSON ResultReuseInformation where
  parseJSON =
    Core.withObject
      "ResultReuseInformation"
      ( \x ->
          ResultReuseInformation'
            Prelude.<$> (x Core..: "ReusedPreviousResult")
      )

instance Prelude.Hashable ResultReuseInformation where
  hashWithSalt _salt ResultReuseInformation' {..} =
    _salt `Prelude.hashWithSalt` reusedPreviousResult

instance Prelude.NFData ResultReuseInformation where
  rnf ResultReuseInformation' {..} =
    Prelude.rnf reusedPreviousResult
