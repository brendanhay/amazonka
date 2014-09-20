{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Requests a refresh of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks. The response contains a
-- RefreshTrustedAdvisorCheckResult object, which contains these fields:
-- Status. The refresh status of the check: "none", "enqueued", "processing",
-- "success", or "abandoned". MillisUntilNextRefreshable. The amount of time,
-- in milliseconds, until the check is eligible for refresh. CheckId. The
-- unique identifier for the check.
module Network.AWS.Support.RefreshTrustedAdvisorCheck
    (
    -- * Request
      RefreshTrustedAdvisorCheck
    -- ** Request constructor
    , refreshTrustedAdvisorCheck
    -- ** Request lenses
    , rtacCheckId

    -- * Response
    , RefreshTrustedAdvisorCheckResponse
    -- ** Response constructor
    , refreshTrustedAdvisorCheckResponse
    -- ** Response lenses
    , rtacrStatus
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck
    { _rtacCheckId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RefreshTrustedAdvisorCheck' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckId ::@ @Text@
--
refreshTrustedAdvisorCheck :: Text -- ^ 'rtacCheckId'
                           -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck p1 = RefreshTrustedAdvisorCheck
    { _rtacCheckId = p1
    }

-- | The unique identifier for the Trusted Advisor check.
rtacCheckId :: Lens' RefreshTrustedAdvisorCheck Text
rtacCheckId = lens _rtacCheckId (\s a -> s { _rtacCheckId = a })

instance ToPath RefreshTrustedAdvisorCheck

instance ToQuery RefreshTrustedAdvisorCheck

instance ToHeaders RefreshTrustedAdvisorCheck

instance ToJSON RefreshTrustedAdvisorCheck

-- | The current refresh status of a Trusted Advisor check.
newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse
    { _rtacrStatus :: TrustedAdvisorCheckRefreshStatus
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RefreshTrustedAdvisorCheckResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @TrustedAdvisorCheckRefreshStatus@
--
refreshTrustedAdvisorCheckResponse :: TrustedAdvisorCheckRefreshStatus -- ^ 'rtacrStatus'
                                   -> RefreshTrustedAdvisorCheckResponse
refreshTrustedAdvisorCheckResponse p1 = RefreshTrustedAdvisorCheckResponse
    { _rtacrStatus = p1
    }

-- | The current refresh status for a check, including the amount of time until
-- the check is eligible for refresh.
rtacrStatus :: Lens' RefreshTrustedAdvisorCheckResponse TrustedAdvisorCheckRefreshStatus
rtacrStatus = lens _rtacrStatus (\s a -> s { _rtacrStatus = a })

instance FromJSON RefreshTrustedAdvisorCheckResponse

instance AWSRequest RefreshTrustedAdvisorCheck where
    type Sv RefreshTrustedAdvisorCheck = Support
    type Rs RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheckResponse

    request = get
    response _ = jsonResponse
