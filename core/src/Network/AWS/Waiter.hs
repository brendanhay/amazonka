-- Module      : Network.AWS.Waiter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Waiter where

import Control.Lens
import Network.AWS.Types
import Prelude           hiding (error)

data Accept
    = Success
    | Failure
    | Retry
      deriving (Eq, Show)

type Acceptor a = Status -> Response a -> Accept

-- | Timing and acceptance criteria to check fulfillment of a remote operation.
data Wait a = Wait
    { _waitName      :: !ByteString
    , _waitAttempts  :: !Int
    , _waitDelay     :: !Int
    , _waitAcceptors :: [Acceptor a]
    }

-- for acceptor in acceptors:
--     if acceptor.matcher_func(response):
--         current_state = acceptor.state
--         break
-- if current_state == 'success':
--     return
-- if current_state == 'failure':
--     raise WaiterError(

-- path :: 

-- pathAll :: Traversal' a b ->

pathAny :: Traversal' (Rs a) b -> Accept -> b -> Acceptor a
pathAny = undefined

-- status

-- error
