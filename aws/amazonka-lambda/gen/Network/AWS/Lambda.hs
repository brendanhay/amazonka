-- Module      : Network.AWS.Lambda
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AWS Lambda is a compute service that runs your code in response to events
-- and automatically manages the compute resources for you, making it easy to
-- build applications that respond quickly to new information. AWS Lambda
-- starts running your code within milliseconds of an event such as an image
-- upload, in-app activity, website click, or output from a connected device.
-- You can also use AWS Lambda to create new back-end services where compute
-- resources are automatically triggered based on custom requests. With AWS
-- Lambda you pay only for the requests served and the compute time required
-- to run your code. Billing is metered in increments of 100 milliseconds,
-- making it cost-effective and easy to scale automatically from a few
-- requests per day to thousands per second.
module Network.AWS.Lambda
    ( module Network.AWS.Lambda.AddEventSource
    , module Network.AWS.Lambda.DeleteFunction
    , module Network.AWS.Lambda.GetEventSource
    , module Network.AWS.Lambda.GetFunction
    , module Network.AWS.Lambda.GetFunctionConfiguration
    , module Network.AWS.Lambda.InvokeAsync
    , module Network.AWS.Lambda.ListEventSources
    , module Network.AWS.Lambda.ListFunctions
    , module Network.AWS.Lambda.RemoveEventSource
    , module Network.AWS.Lambda.Types
    , module Network.AWS.Lambda.UpdateFunctionConfiguration
    , module Network.AWS.Lambda.UploadFunction
    ) where

import Network.AWS.Lambda.AddEventSource
import Network.AWS.Lambda.DeleteFunction
import Network.AWS.Lambda.GetEventSource
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.InvokeAsync
import Network.AWS.Lambda.ListEventSources
import Network.AWS.Lambda.ListFunctions
import Network.AWS.Lambda.RemoveEventSource
import Network.AWS.Lambda.Types
import Network.AWS.Lambda.UpdateFunctionConfiguration
import Network.AWS.Lambda.UploadFunction
