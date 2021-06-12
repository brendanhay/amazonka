{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.Lens
import Network.AWS.Route53.Types

-- | Polls 'Network.AWS.Route53.GetChange' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newResourceRecordSetsChanged :: Core.Wait GetChange
newResourceRecordSetsChanged =
  Core.Wait
    { Core._waitName =
        "ResourceRecordSetsChanged",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "INSYNC"
            Core.AcceptSuccess
            ( getChangeResponse_changeInfo
                Core.. changeInfo_status
                Core.. Lens.to Core.toTextCI
            )
        ]
    }
