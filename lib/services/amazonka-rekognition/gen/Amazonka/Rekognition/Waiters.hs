{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rekognition.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.DescribeProjectVersions
import Amazonka.Rekognition.Lens
import Amazonka.Rekognition.Types

-- | Polls 'Amazonka.Rekognition.DescribeProjectVersions' every 120 seconds until a successful state is reached. An error is returned after 360 failed checks.
newProjectVersionTrainingCompleted :: Core.Wait DescribeProjectVersions
newProjectVersionTrainingCompleted =
  Core.Wait
    { Core.name =
        "ProjectVersionTrainingCompleted",
      Core.attempts = 360,
      Core.delay = 120,
      Core.acceptors =
        [ Core.matchAll
            "TRAINING_COMPLETED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "TRAINING_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Rekognition.DescribeProjectVersions' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newProjectVersionRunning :: Core.Wait DescribeProjectVersions
newProjectVersionRunning =
  Core.Wait
    { Core.name = "ProjectVersionRunning",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeProjectVersionsResponse_projectVersionDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. projectVersionDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
