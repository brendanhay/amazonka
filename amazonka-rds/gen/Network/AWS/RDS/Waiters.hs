{-# LANGUAGE TypeFamilies #-}

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
import Network.AWS.Types

dBInstanceAvailable :: Wait DescribeDBInstances
dBInstanceAvailable = Wait
    { _waitDelay     = 30
    , _waitAttempts  = 60
    , _waitAccept    = const True
    }

dBInstanceDeleted :: Wait DescribeDBInstances
dBInstanceDeleted = Wait
    { _waitDelay     = 30
    , _waitAttempts  = 60
    , _waitAccept    = const True
    }
