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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.Lens
import Network.AWS.Route53.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Route53.GetChange' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newResourceRecordSetsChanged :: Waiter.Wait GetChange
newResourceRecordSetsChanged =
  Waiter.Wait
    { Waiter._waitName =
        "ResourceRecordSetsChanged",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "INSYNC"
            Waiter.AcceptSuccess
            ( getChangeResponse_changeInfo
                Prelude.. changeInfo_status
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
