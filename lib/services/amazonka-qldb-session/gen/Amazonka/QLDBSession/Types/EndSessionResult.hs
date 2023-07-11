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
-- Module      : Amazonka.QLDBSession.Types.EndSessionResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.EndSessionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the ended session.
--
-- /See:/ 'newEndSessionResult' smart constructor.
data EndSessionResult = EndSessionResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndSessionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timingInformation', 'endSessionResult_timingInformation' - Contains server-side performance information for the command.
newEndSessionResult ::
  EndSessionResult
newEndSessionResult =
  EndSessionResult'
    { timingInformation =
        Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
endSessionResult_timingInformation :: Lens.Lens' EndSessionResult (Prelude.Maybe TimingInformation)
endSessionResult_timingInformation = Lens.lens (\EndSessionResult' {timingInformation} -> timingInformation) (\s@EndSessionResult' {} a -> s {timingInformation = a} :: EndSessionResult)

instance Data.FromJSON EndSessionResult where
  parseJSON =
    Data.withObject
      "EndSessionResult"
      ( \x ->
          EndSessionResult'
            Prelude.<$> (x Data..:? "TimingInformation")
      )

instance Prelude.Hashable EndSessionResult where
  hashWithSalt _salt EndSessionResult' {..} =
    _salt `Prelude.hashWithSalt` timingInformation

instance Prelude.NFData EndSessionResult where
  rnf EndSessionResult' {..} =
    Prelude.rnf timingInformation
