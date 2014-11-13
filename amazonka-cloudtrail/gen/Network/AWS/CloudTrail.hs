-- Module      : Network.AWS.CloudTrail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | CloudTrail is a web service that records AWS API calls for your AWS account
-- and delivers log files to an Amazon S3 bucket. The recorded information
-- includes the identity of the user, the start time of the AWS API call, the
-- source IP address, the request parameters, and the response elements
-- returned by the service.
module Network.AWS.CloudTrail
    ( module Network.AWS.CloudTrail.CreateTrail
    , module Network.AWS.CloudTrail.DeleteTrail
    , module Network.AWS.CloudTrail.DescribeTrails
    , module Network.AWS.CloudTrail.GetTrailStatus
    , module Network.AWS.CloudTrail.StartLogging
    , module Network.AWS.CloudTrail.StopLogging
    , module Network.AWS.CloudTrail.Types
    , module Network.AWS.CloudTrail.UpdateTrail
    ) where

import Network.AWS.CloudTrail.CreateTrail
import Network.AWS.CloudTrail.DeleteTrail
import Network.AWS.CloudTrail.DescribeTrails
import Network.AWS.CloudTrail.GetTrailStatus
import Network.AWS.CloudTrail.StartLogging
import Network.AWS.CloudTrail.StopLogging
import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.UpdateTrail
