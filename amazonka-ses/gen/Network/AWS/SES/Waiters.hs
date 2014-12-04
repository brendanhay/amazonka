{-# LANGUAGE TypeFamilies #-}

-- Module      : Network.AWS.SES.Waiters
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

module Network.AWS.SES.Waiters where

import Network.AWS.SES.GetIdentityVerificationAttributes
import Network.AWS.SES.Types
import Network.AWS.Types

data IdentityExists = IdentityExists

instance AWSWaiter IdentityExists where
    type Rq IdentityExists = GetIdentityVerificationAttributes

    waiter IdentityExists x = Waiter
        { _waitDelay     = 3
        , _waitAttempts  = 20
        , _waitOperation = x
        , _waitAccept    = const False
        }
