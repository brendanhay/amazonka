-- Module      : Network.AWS.Lambda
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

-- | AWS Lambda is a compute service that runs your code in response to events and
-- automatically manages the compute resources for you, making it easy to build
-- applications that respond quickly to new information. AWS Lambda starts
-- running your code within milliseconds of an event such as an image upload,
-- in-app activity, website click, or output from a connected device. You can
-- also use AWS Lambda to create new back-end services where compute resources
-- are automatically triggered based on custom requests. With AWS Lambda you pay
-- only for the requests served and the compute time required to run your code.
-- Billing is metered in increments of 100 milliseconds, making it
-- cost-effective and easy to scale automatically from a few requests per day to
-- thousands per second.
module Network.AWS.Lambda
    ( module Network.AWS.Lambda.AddPermission
    , module Network.AWS.Lambda.CreateEventSourceMapping
    , module Network.AWS.Lambda.CreateFunction
    , module Network.AWS.Lambda.DeleteEventSourceMapping
    , module Network.AWS.Lambda.DeleteFunction
    , module Network.AWS.Lambda.GetEventSourceMapping
    , module Network.AWS.Lambda.GetFunction
    , module Network.AWS.Lambda.GetFunctionConfiguration
    , module Network.AWS.Lambda.GetPolicy
    , module Network.AWS.Lambda.Invoke
    , module Network.AWS.Lambda.InvokeAsync
    , module Network.AWS.Lambda.ListEventSourceMappings
    , module Network.AWS.Lambda.ListFunctions
    , module Network.AWS.Lambda.RemovePermission
    , module Network.AWS.Lambda.Types
    , module Network.AWS.Lambda.UpdateEventSourceMapping
    , module Network.AWS.Lambda.UpdateFunctionCode
    , module Network.AWS.Lambda.UpdateFunctionConfiguration
    ) where

import Network.AWS.Lambda.AddPermission
import Network.AWS.Lambda.CreateEventSourceMapping
import Network.AWS.Lambda.CreateFunction
import Network.AWS.Lambda.DeleteEventSourceMapping
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.GetEventSourceMapping
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.GetPolicy
import Network.AWS.Lambda.Invoke
import Network.AWS.Lambda.InvokeAsync
import Network.AWS.Lambda.ListEventSourceMappings
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.RemovePermission
import Network.AWS.Lambda.Types
import Network.AWS.Lambda.UpdateEventSourceMapping
import Network.AWS.Lambda.UpdateFunctionCode
import Network.AWS.Lambda.UpdateFunctionConfiguration
