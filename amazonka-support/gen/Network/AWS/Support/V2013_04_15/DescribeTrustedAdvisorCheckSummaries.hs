{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the summaries of the results of the Trusted Advisor checks that
-- have the specified check IDs. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks. The response contains an array of
-- TrustedAdvisorCheckSummary objects.
module Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckSummaries
    (
    -- * Request
      DescribeTrustedAdvisorCheckSummaries
    -- ** Request constructor
    , mkDescribeTrustedAdvisorCheckSummaries
    -- ** Request lenses
    , dtacsCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckSummariesResponse
    -- ** Response lenses
    , dtacsrSummaries
    ) where

import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | 
newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries
    { _dtacsCheckIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTrustedAdvisorCheckSummaries' request.
mkDescribeTrustedAdvisorCheckSummaries :: [Text] -- ^ 'dtacsCheckIds'
                                       -> DescribeTrustedAdvisorCheckSummaries
mkDescribeTrustedAdvisorCheckSummaries p1 = DescribeTrustedAdvisorCheckSummaries
    { _dtacsCheckIds = p1
    }

-- | The IDs of the Trusted Advisor checks.
dtacsCheckIds :: Lens' DescribeTrustedAdvisorCheckSummaries [Text]
dtacsCheckIds = lens _dtacsCheckIds (\s a -> s { _dtacsCheckIds = a })

instance ToPath DescribeTrustedAdvisorCheckSummaries

instance ToQuery DescribeTrustedAdvisorCheckSummaries

instance ToHeaders DescribeTrustedAdvisorCheckSummaries

instance ToJSON DescribeTrustedAdvisorCheckSummaries

-- | The summaries of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckSummaries operation.
newtype DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse
    { _dtacsrSummaries :: [TrustedAdvisorCheckSummary]
    } deriving (Show, Generic)

-- | The summary information for the requested Trusted Advisor checks.
dtacsrSummaries :: Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
dtacsrSummaries = lens _dtacsrSummaries (\s a -> s { _dtacsrSummaries = a })

instance FromJSON DescribeTrustedAdvisorCheckSummariesResponse

instance AWSRequest DescribeTrustedAdvisorCheckSummaries where
    type Sv DescribeTrustedAdvisorCheckSummaries = Support
    type Rs DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummariesResponse

    request = get
    response _ = jsonResponse
