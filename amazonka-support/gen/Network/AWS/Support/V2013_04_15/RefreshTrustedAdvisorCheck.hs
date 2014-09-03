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
    , refreshTrustedAdvisorCheck
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

-- | Minimum specification for a 'RefreshTrustedAdvisorCheck' request.
refreshTrustedAdvisorCheck :: Text -- ^ 'rtacrCheckId'
                           -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck p1 = RefreshTrustedAdvisorCheck
    { _rtacrCheckId = p1
    }

data RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck
    { _rtacrCheckId :: Text
      -- ^ The unique identifier for the Trusted Advisor check.
    } deriving (Show, Generic)

-- | The unique identifier for the Trusted Advisor check.
rtacrCheckId
    :: Functor f
    => (Text
    -> f (Text))
    -> RefreshTrustedAdvisorCheck
    -> f RefreshTrustedAdvisorCheck
rtacrCheckId f x =
    (\y -> x { _rtacrCheckId = y })
       <$> f (_rtacrCheckId x)
{-# INLINE rtacrCheckId #-}

instance ToPath RefreshTrustedAdvisorCheck

instance ToQuery RefreshTrustedAdvisorCheck

instance ToHeaders RefreshTrustedAdvisorCheck

instance ToJSON RefreshTrustedAdvisorCheck

data RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse
    { _rtacsStatus :: TrustedAdvisorCheckRefreshStatus
      -- ^ The current refresh status for a check, including the amount of
      -- time until the check is eligible for refresh.
    } deriving (Show, Generic)

-- | The current refresh status for a check, including the amount of time until
-- the check is eligible for refresh.
rtacsStatus
    :: Functor f
    => (TrustedAdvisorCheckRefreshStatus
    -> f (TrustedAdvisorCheckRefreshStatus))
    -> RefreshTrustedAdvisorCheckResponse
    -> f RefreshTrustedAdvisorCheckResponse
rtacsStatus f x =
    (\y -> x { _rtacsStatus = y })
       <$> f (_rtacsStatus x)
{-# INLINE rtacsStatus #-}

instance FromJSON RefreshTrustedAdvisorCheckResponse

instance AWSRequest RefreshTrustedAdvisorCheck where
    type Sv RefreshTrustedAdvisorCheck = Support
    type Rs RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheckResponse

    request = get
    response _ = jsonResponse
