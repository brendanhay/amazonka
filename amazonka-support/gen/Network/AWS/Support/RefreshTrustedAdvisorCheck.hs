{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a refresh of the Trusted Advisor check that has the specified check
-- ID. Check IDs can be obtained by calling 'DescribeTrustedAdvisorChecks'.
--
-- The response contains a 'TrustedAdvisorCheckRefreshStatus' object, which
-- contains these fields:
--
-- Status. The refresh status of the check: "none", "enqueued", "processing",
-- "success", or "abandoned".  MillisUntilNextRefreshable. The amount of time,
-- in milliseconds, until the check is eligible for refresh.  CheckId. The
-- unique identifier for the check.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_RefreshTrustedAdvisorCheck.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck
    { _rtacCheckId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'RefreshTrustedAdvisorCheck' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacCheckId' @::@ 'Text'
--
refreshTrustedAdvisorCheck :: Text -- ^ 'rtacCheckId'
                           -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck p1 = RefreshTrustedAdvisorCheck
    { _rtacCheckId = p1
    }

-- | The unique identifier for the Trusted Advisor check.
rtacCheckId :: Lens' RefreshTrustedAdvisorCheck Text
rtacCheckId = lens _rtacCheckId (\s a -> s { _rtacCheckId = a })

newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse
    { _rtacrStatus :: TrustedAdvisorCheckRefreshStatus
    } deriving (Eq, Read, Show)

-- | 'RefreshTrustedAdvisorCheckResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacrStatus' @::@ 'TrustedAdvisorCheckRefreshStatus'
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

instance ToPath RefreshTrustedAdvisorCheck where
    toPath = const "/"

instance ToQuery RefreshTrustedAdvisorCheck where
    toQuery = const mempty

instance ToHeaders RefreshTrustedAdvisorCheck

instance ToJSON RefreshTrustedAdvisorCheck where
    toJSON RefreshTrustedAdvisorCheck{..} = object
        [ "checkId" .= _rtacCheckId
        ]

instance AWSRequest RefreshTrustedAdvisorCheck where
    type Sv RefreshTrustedAdvisorCheck = Support
    type Rs RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheckResponse

    request  = post "RefreshTrustedAdvisorCheck"
    response = jsonResponse

instance FromJSON RefreshTrustedAdvisorCheckResponse where
    parseJSON = withObject "RefreshTrustedAdvisorCheckResponse" $ \o -> RefreshTrustedAdvisorCheckResponse
        <$> o .:  "status"
