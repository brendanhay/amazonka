-- Module      : Network.AWS.CloudTrail.V2013_11_01
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
module Network.AWS.CloudTrail.V2013_11_01
    ( module Network.AWS.CloudTrail.V2013_11_01.CreateTrail
    , module Network.AWS.CloudTrail.V2013_11_01.DeleteTrail
    , module Network.AWS.CloudTrail.V2013_11_01.DescribeTrails
    , module Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus
    , module Network.AWS.CloudTrail.V2013_11_01.StartLogging
    , module Network.AWS.CloudTrail.V2013_11_01.StopLogging
    , module Network.AWS.CloudTrail.V2013_11_01.Types
    , module Network.AWS.CloudTrail.V2013_11_01.UpdateTrail
    ) where

import Network.AWS.CloudTrail.V2013_11_01.CreateTrail
import Network.AWS.CloudTrail.V2013_11_01.DeleteTrail
import Network.AWS.CloudTrail.V2013_11_01.DescribeTrails
import Network.AWS.CloudTrail.V2013_11_01.GetTrailStatus
import Network.AWS.CloudTrail.V2013_11_01.StartLogging
import Network.AWS.CloudTrail.V2013_11_01.StopLogging
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.CloudTrail.V2013_11_01.UpdateTrail
