{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
