{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.GetSensitiveDataOccurrences
import Amazonka.MacieV2.Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.MacieV2.GetSensitiveDataOccurrences' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFindingRevealed :: Core.Wait GetSensitiveDataOccurrences
newFindingRevealed =
  Core.Wait
    { Core.name = "FindingRevealed",
      Core.attempts = 60,
      Core.delay = 2,
      Core.acceptors =
        [ Core.matchAll
            "SUCCESS"
            Core.AcceptSuccess
            ( getSensitiveDataOccurrencesResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ERROR"
            Core.AcceptSuccess
            ( getSensitiveDataOccurrencesResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
