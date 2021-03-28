{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Waiters
  (
    -- * StreamExists
    mkStreamExists,
    -- * StreamNotExists
    mkStreamNotExists,
  ) where

import Network.AWS.Kinesis.DescribeStream
import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
mkStreamExists :: Waiter.Wait DescribeStream
mkStreamExists
  = Waiter.Wait{Waiter._waitName = "StreamExists",
                Waiter._waitAttempts = 18, Waiter._waitDelay = 10,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "ACTIVE" Waiter.AcceptSuccess
                     (Lens.field @"streamDescription" Core..
                        Lens.field @"streamStatus")]}

-- | Polls 'Network.AWS.Kinesis.DescribeStream' every 10 seconds until a successful state is reached. An error is returned after 18 failed checks.
mkStreamNotExists :: Waiter.Wait DescribeStream
mkStreamNotExists
  = Waiter.Wait{Waiter._waitName = "StreamNotExists",
                Waiter._waitAttempts = 18, Waiter._waitDelay = 10,
                Waiter._waitAcceptors =
                  [Waiter.matchError "ResourceNotFoundException"
                     Waiter.AcceptSuccess]}
