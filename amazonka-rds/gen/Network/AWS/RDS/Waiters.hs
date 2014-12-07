{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.RDS.Waiters where

import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.Types
import Network.AWS.Waiters

dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable = Wait
    { _waitName      = "DBInstanceAvailable"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ matchAll "available" AcceptSuccess
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "deleted" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "deleting" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "failed" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "incompatible-restore" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "incompatible-parameters" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "incompatible-parameters" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "incompatible-restore" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        ]
    }

dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted = Wait
    { _waitName      = "DBInstanceDeleted"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ matchAll "deleted" AcceptSuccess
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "creating" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "modifying" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "rebooting" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        , matchAny "resetting-master-credentials" AcceptFailure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus . _Just)
        ]
    }
