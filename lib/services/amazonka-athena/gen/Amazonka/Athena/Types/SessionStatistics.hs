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
-- Module      : Amazonka.Athena.Types.SessionStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.SessionStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains statistics for a notebook session.
--
-- /See:/ 'newSessionStatistics' smart constructor.
data SessionStatistics = SessionStatistics'
  { -- | The data processing unit execution time for a session in milliseconds.
    dpuExecutionInMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dpuExecutionInMillis', 'sessionStatistics_dpuExecutionInMillis' - The data processing unit execution time for a session in milliseconds.
newSessionStatistics ::
  SessionStatistics
newSessionStatistics =
  SessionStatistics'
    { dpuExecutionInMillis =
        Prelude.Nothing
    }

-- | The data processing unit execution time for a session in milliseconds.
sessionStatistics_dpuExecutionInMillis :: Lens.Lens' SessionStatistics (Prelude.Maybe Prelude.Integer)
sessionStatistics_dpuExecutionInMillis = Lens.lens (\SessionStatistics' {dpuExecutionInMillis} -> dpuExecutionInMillis) (\s@SessionStatistics' {} a -> s {dpuExecutionInMillis = a} :: SessionStatistics)

instance Data.FromJSON SessionStatistics where
  parseJSON =
    Data.withObject
      "SessionStatistics"
      ( \x ->
          SessionStatistics'
            Prelude.<$> (x Data..:? "DpuExecutionInMillis")
      )

instance Prelude.Hashable SessionStatistics where
  hashWithSalt _salt SessionStatistics' {..} =
    _salt `Prelude.hashWithSalt` dpuExecutionInMillis

instance Prelude.NFData SessionStatistics where
  rnf SessionStatistics' {..} =
    Prelude.rnf dpuExecutionInMillis
