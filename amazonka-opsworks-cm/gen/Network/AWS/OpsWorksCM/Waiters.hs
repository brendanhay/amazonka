{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Waiters where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus
import Network.AWS.OpsWorksCM.Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.OpsWorksCM.DescribeNodeAssociationStatus' every 15 seconds until a successful state is reached. An error is returned after 15 failed checks.
newNodeAssociated :: Waiter.Wait DescribeNodeAssociationStatus
newNodeAssociated =
  Waiter.Wait
    { Waiter._waitName = "NodeAssociated",
      Waiter._waitAttempts = 15,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "SUCCESS"
            Waiter.AcceptSuccess
            ( describeNodeAssociationStatusResponse_nodeAssociationStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeNodeAssociationStatusResponse_nodeAssociationStatus
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
