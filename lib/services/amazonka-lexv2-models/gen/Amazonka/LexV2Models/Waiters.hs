{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.DescribeBot
import Amazonka.LexV2Models.DescribeBotAlias
import Amazonka.LexV2Models.DescribeBotLocale
import Amazonka.LexV2Models.DescribeBotVersion
import Amazonka.LexV2Models.DescribeExport
import Amazonka.LexV2Models.DescribeImport
import Amazonka.LexV2Models.Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleBuilt :: Core.Wait DescribeBotLocale
newBotLocaleBuilt =
  Core.Wait
    { Core.name = "BotLocaleBuilt",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeImport' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotImportCompleted :: Core.Wait DescribeImport
newBotImportCompleted =
  Core.Wait
    { Core.name = "BotImportCompleted",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeImportResponse_importStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleExpressTestingAvailable :: Core.Wait DescribeBotLocale
newBotLocaleExpressTestingAvailable =
  Core.Wait
    { Core.name =
        "BotLocaleExpressTestingAvailable",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ReadyExpressTesting"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeBot' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotAvailable :: Core.Wait DescribeBot
newBotAvailable =
  Core.Wait
    { Core.name = "BotAvailable",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Inactive"
            Core.AcceptFailure
            ( describeBotResponse_botStatus Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeBotAlias' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotAliasAvailable :: Core.Wait DescribeBotAlias
newBotAliasAvailable =
  Core.Wait
    { Core.name = "BotAliasAvailable",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotAliasResponse_botAliasStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeBotLocale' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotLocaleCreated :: Core.Wait DescribeBotLocale
newBotLocaleCreated =
  Core.Wait
    { Core.name = "BotLocaleCreated",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Built"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "ReadyExpressTesting"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "NotBuilt"
            Core.AcceptSuccess
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotLocaleResponse_botLocaleStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeBotVersion' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotVersionAvailable :: Core.Wait DescribeBotVersion
newBotVersionAvailable =
  Core.Wait
    { Core.name = "BotVersionAvailable",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Available"
            Core.AcceptSuccess
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeBotVersionResponse_botStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.LexV2Models.DescribeExport' every 10 seconds until a successful state is reached. An error is returned after 35 failed checks.
newBotExportCompleted :: Core.Wait DescribeExport
newBotExportCompleted =
  Core.Wait
    { Core.name = "BotExportCompleted",
      Core.attempts = 35,
      Core.delay = 10,
      Core.acceptors =
        [ Core.matchAll
            "Completed"
            Core.AcceptSuccess
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Deleting"
            Core.AcceptFailure
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( describeExportResponse_exportStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
