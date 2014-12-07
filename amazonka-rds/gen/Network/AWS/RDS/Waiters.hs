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
        [ matchAll "available" Success
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "deleted" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "deleting" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "failed" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "incompatible-restore" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "incompatible-parameters" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "incompatible-parameters" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "incompatible-restore" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        ]
    }

dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted = Wait
    { _waitName      = "DBInstanceDeleted"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ matchAll "deleted" Success
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "creating" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "modifying" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "rebooting" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        , matchAny "resetting-master-credentials" Failure
            (folding (concatOf ddbirDBInstances) . dbiDBInstanceStatus)
        ]
    }
