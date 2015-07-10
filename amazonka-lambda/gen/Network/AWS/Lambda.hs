{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | AWS Lambda
--
-- __Overview__
--
-- This is the /AWS Lambda API Reference/. The AWS Lambda Developer Guide
-- provides additional information. For the service overview, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/welcome.html What is AWS Lambda>,
-- and for information about how the service works, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>
-- in the /AWS Lambda Developer Guide/.
module Network.AWS.Lambda
    ( module Export
    ) where

import           Network.AWS.Lambda.AddPermission               as Export
import           Network.AWS.Lambda.CreateEventSourceMapping    as Export
import           Network.AWS.Lambda.CreateFunction              as Export
import           Network.AWS.Lambda.DeleteEventSourceMapping    as Export
import           Network.AWS.Lambda.DeleteFunction              as Export
import           Network.AWS.Lambda.GetEventSourceMapping       as Export
import           Network.AWS.Lambda.GetFunction                 as Export
import           Network.AWS.Lambda.GetFunctionConfiguration    as Export
import           Network.AWS.Lambda.GetPolicy                   as Export
import           Network.AWS.Lambda.Invoke                      as Export
import           Network.AWS.Lambda.ListEventSourceMappings     as Export
import           Network.AWS.Lambda.ListFunctions               as Export
import           Network.AWS.Lambda.RemovePermission            as Export
import           Network.AWS.Lambda.Types                       as Export
import           Network.AWS.Lambda.UpdateEventSourceMapping    as Export
import           Network.AWS.Lambda.UpdateFunctionCode          as Export
import           Network.AWS.Lambda.UpdateFunctionConfiguration as Export
import           Network.AWS.Lambda.Waiters                     as Export
