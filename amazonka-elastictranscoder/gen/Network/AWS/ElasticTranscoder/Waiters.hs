{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.Waiters
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

module Network.AWS.ElasticTranscoder.Waiters where

import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.Waiter

jobComplete :: Wait ReadJob
jobComplete = Wait
    { _waitName      = "JobComplete"
    , _waitAttempts  = 120
    , _waitDelay     = 30
    , _waitAcceptors =
        [ path Success {"contents":["rjrJob",{"contents":"jStatus","type":"access"}],"type":"nested"} "Complete"
        , path Failure {"contents":["rjrJob",{"contents":"jStatus","type":"access"}],"type":"nested"} "Canceled"
        , path Failure {"contents":["rjrJob",{"contents":"jStatus","type":"access"}],"type":"nested"} "Error"
        ]
    }
