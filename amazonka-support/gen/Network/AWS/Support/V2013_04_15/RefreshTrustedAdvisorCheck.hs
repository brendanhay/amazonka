{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.RefreshTrustedAdvisorCheck
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
module Network.AWS.Support.V2013_04_15.RefreshTrustedAdvisorCheck
    (
    -- * Request
      RefreshTrustedAdvisorCheck
    -- ** Request constructor
    , mkRefreshTrustedAdvisorCheckRequest
    -- ** Request lenses
    , rtacrCheckId

    -- * Response
    , RefreshTrustedAdvisorCheckResponse
    -- ** Response lenses
    , rtacsStatus
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RefreshTrustedAdvisorCheck' request.
mkRefreshTrustedAdvisorCheckRequest :: Text -- ^ 'rtacrCheckId'
                                    -> RefreshTrustedAdvisorCheck
mkRefreshTrustedAdvisorCheckRequest p1 = RefreshTrustedAdvisorCheck
    { _rtacrCheckId = p1
    }
{-# INLINE mkRefreshTrustedAdvisorCheckRequest #-}

newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck
    { _rtacrCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
rtacrCheckId :: Lens' RefreshTrustedAdvisorCheck (Text)
rtacrCheckId = lens _rtacrCheckId (\s a -> s { _rtacrCheckId = a })
{-# INLINE rtacrCheckId #-}

instance ToPath RefreshTrustedAdvisorCheck

instance ToQuery RefreshTrustedAdvisorCheck

instance ToHeaders RefreshTrustedAdvisorCheck

instance ToJSON RefreshTrustedAdvisorCheck

newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse
    { _rtacsStatus :: TrustedAdvisorCheckRefreshStatus
      -- ^ The current refresh status for a check, including the amount of
      -- time until the check is eligible for refresh.
    } deriving (Show, Generic)

-- | The current refresh status for a check, including the amount of time until
-- the check is eligible for refresh.
rtacsStatus :: Lens' RefreshTrustedAdvisorCheckResponse (TrustedAdvisorCheckRefreshStatus)
rtacsStatus = lens _rtacsStatus (\s a -> s { _rtacsStatus = a })
{-# INLINE rtacsStatus #-}

instance FromJSON RefreshTrustedAdvisorCheckResponse

instance AWSRequest RefreshTrustedAdvisorCheck where
    type Sv RefreshTrustedAdvisorCheck = Support
    type Rs RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheckResponse

    request = get
    response _ = jsonResponse
