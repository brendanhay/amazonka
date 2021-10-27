{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Models.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.DescribeBot
import Network.AWS.LexV2Models.DescribeBotAlias
import Network.AWS.LexV2Models.DescribeBotLocale
import Network.AWS.LexV2Models.DescribeBotVersion
import Network.AWS.LexV2Models.DescribeExport
import Network.AWS.LexV2Models.DescribeImport
import Network.AWS.LexV2Models.Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleExpressTestingAvailable :: Core.Wait DescribeBotLocale
newBotLocaleExpressTestingAvailable =
  Core.Wait
    { Core._waitName =
        "BotLocaleExpressTestingAvailable",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ReadyExpressTesting"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleBuilt :: Core.Wait DescribeBotLocale
newBotLocaleBuilt =
  Core.Wait
    { Core._waitName = "BotLocaleBuilt",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeBotVersion' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotVersionAvailable :: Core.Wait DescribeBotVersion
newBotVersionAvailable =
  Core.Wait
    { Core._waitName = "BotVersionAvailable",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeBot' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotAvailable :: Core.Wait DescribeBot
newBotAvailable =
  Core.Wait
    { Core._waitName = "BotAvailable",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Inactive"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeExport' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotExportCompleted :: Core.Wait DescribeExport
newBotExportCompleted =
  Core.Wait
    { Core._waitName = "BotExportCompleted",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeImport' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotImportCompleted :: Core.Wait DescribeImport
newBotImportCompleted =
  Core.Wait
    { Core._waitName = "BotImportCompleted",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeBotAlias' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotAliasAvailable :: Core.Wait DescribeBotAlias
newBotAliasAvailable =
  Core.Wait
    { Core._waitName = "BotAliasAvailable",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleCreated :: Core.Wait DescribeBotLocale
newBotLocaleCreated =
  Core.Wait
    { Core._waitName = "BotLocaleCreated",
      Core._waitAttempts = 35,
      Core._waitDelay = 10,
      Core._waitAcceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "ReadyExpressTesting"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
