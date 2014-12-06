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
import Network.AWS.Waiters

dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable = Wait
    { _waitName      = "DBInstanceAvailable"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ pathAll (ddbirDBInstances . dbiDBInstanceStatus) "available" Success
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "deleted" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "deleting" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "failed" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "incompatible-restore" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "incompatible-parameters" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "incompatible-parameters" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "incompatible-restore" Failure
        ]
    }

dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted = Wait
    { _waitName      = "DBInstanceDeleted"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ pathAll (ddbirDBInstances . dbiDBInstanceStatus) "deleted" Success
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "creating" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "modifying" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "rebooting" Failure
        , pathAny (ddbirDBInstances . dbiDBInstanceStatus) "resetting-master-credentials" Failure
        ]
    }
