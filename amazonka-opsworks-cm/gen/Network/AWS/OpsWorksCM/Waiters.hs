{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorksCM.Waiters where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import Network.AWS.OpsWorksCM.Types
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus' every 15 seconds until a successful state is reached. An error is returned after 15 failed checks.
nodeAssociated :: Wait DescribeNodeAssociationStatus
nodeAssociated =
  Wait
    { _waitName = "NodeAssociated"
    , _waitAttempts = 15
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "SUCCESS"
            AcceptSuccess
            (dnasrsNodeAssociationStatus . to toTextCI)
        , matchAll
            "FAILED"
            AcceptFailure
            (dnasrsNodeAssociationStatus . to toTextCI)
        ]
    }

