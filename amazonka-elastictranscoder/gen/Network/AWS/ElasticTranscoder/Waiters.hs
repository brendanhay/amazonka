{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Waiters where

import Network.AWS.ElasticTranscoder.ReadJob
import Network.AWS.ElasticTranscoder.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ElasticTranscoder.ReadJob' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
jobComplete :: Wait ReadJob
jobComplete =
  Wait
    { _waitName = "JobComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "Complete"
            AcceptSuccess
            (rjrsJob . jStatus . _Just . to toTextCI)
        , matchAll
            "Canceled"
            AcceptFailure
            (rjrsJob . jStatus . _Just . to toTextCI)
        , matchAll
            "Error"
            AcceptFailure
            (rjrsJob . jStatus . _Just . to toTextCI)
        ]
    }

