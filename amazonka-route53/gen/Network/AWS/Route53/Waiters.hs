{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.GetChange
import Network.AWS.Route53.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Route53.GetChange' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
resourceRecordSetsChanged :: Wait GetChange
resourceRecordSetsChanged =
  Wait
    { _waitName = "ResourceRecordSetsChanged"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "INSYNC"
            AcceptSuccess
            (gcrsChangeInfo . ciStatus . to toTextCI)
        ]
    }

