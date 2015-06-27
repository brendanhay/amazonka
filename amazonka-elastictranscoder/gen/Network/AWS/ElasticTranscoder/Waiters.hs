{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Network.AWS.ElasticTranscoder.ReadJob
import           Network.AWS.ElasticTranscoder.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

jobComplete :: Wait ReadJob
jobComplete =
    Wait
    { _waitName = "JobComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "Complete"
                             AcceptSuccess
                             (rjrJob . jStatus . _Just . to toText)
                       , matchAll
                             "Canceled"
                             AcceptFailure
                             (rjrJob . jStatus . _Just . to toText)
                       , matchAll
                             "Error"
                             AcceptFailure
                             (rjrJob . jStatus . _Just . to toText)]
    }
