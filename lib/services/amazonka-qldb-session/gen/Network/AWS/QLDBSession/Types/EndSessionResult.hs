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
-- Module      : Network.AWS.QLDBSession.Types.EndSessionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDBSession.Types.EndSessionResult where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDBSession.Types.TimingInformation

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

instance Core.FromJSON EndSessionResult where
  parseJSON =
    Core.withObject
      "EndSessionResult"
      ( \x ->
          EndSessionResult'
            Prelude.<$> (x Core..:? "TimingInformation")
      )

instance Prelude.Hashable EndSessionResult

instance Prelude.NFData EndSessionResult
