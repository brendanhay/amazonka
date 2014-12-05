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
import Network.AWS.Waiter

dbInstanceAvailable :: Wait DescribeDBInstances
dbInstanceAvailable = Wait
    { _waitName      = "DBInstanceAvailable"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ pathAll Success {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "available"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "deleted"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "deleting"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "failed"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "incompatible-restore"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "incompatible-parameters"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "incompatible-parameters"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "incompatible-restore"
        ]
    }

dbInstanceDeleted :: Wait DescribeDBInstances
dbInstanceDeleted = Wait
    { _waitName      = "DBInstanceDeleted"
    , _waitAttempts  = 60
    , _waitDelay     = 30
    , _waitAcceptors =
        [ pathAll Success {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "deleted"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "creating"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "modifying"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "rebooting"
        , pathAny Failure {"contents":["ddbirDBInstances",{"contents":"dbiDBInstanceStatus","type":"access"}],"type":"indexed"} "resetting-master-credentials"
        ]
    }
