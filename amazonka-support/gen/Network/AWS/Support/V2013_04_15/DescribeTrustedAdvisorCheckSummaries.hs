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
    , mkDescribeTrustedAdvisorCheckSummariesRequest
    -- ** Request lenses
    , dtacsrCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckSummariesResponse
    -- ** Response lenses
    , dtacssSummaries
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTrustedAdvisorCheckSummaries' request.
mkDescribeTrustedAdvisorCheckSummariesRequest :: [Text] -- ^ 'dtacsrCheckIds'
                                              -> DescribeTrustedAdvisorCheckSummaries
mkDescribeTrustedAdvisorCheckSummariesRequest p1 = DescribeTrustedAdvisorCheckSummaries
    { _dtacsrCheckIds = p1
    }
{-# INLINE mkDescribeTrustedAdvisorCheckSummariesRequest #-}

newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries
    { _dtacsrCheckIds :: [Text]
      -- ^ The IDs of the Trusted Advisor checks.
    } deriving (Show, Generic)

-- | The IDs of the Trusted Advisor checks.
dtacsrCheckIds :: Lens' DescribeTrustedAdvisorCheckSummaries ([Text])
dtacsrCheckIds = lens _dtacsrCheckIds (\s a -> s { _dtacsrCheckIds = a })
{-# INLINE dtacsrCheckIds #-}

instance ToPath DescribeTrustedAdvisorCheckSummaries

instance ToQuery DescribeTrustedAdvisorCheckSummaries

instance ToHeaders DescribeTrustedAdvisorCheckSummaries

instance ToJSON DescribeTrustedAdvisorCheckSummaries

newtype DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse
    { _dtacssSummaries :: [TrustedAdvisorCheckSummary]
      -- ^ The summary information for the requested Trusted Advisor checks.
    } deriving (Show, Generic)

-- | The summary information for the requested Trusted Advisor checks.
dtacssSummaries :: Lens' DescribeTrustedAdvisorCheckSummariesResponse ([TrustedAdvisorCheckSummary])
dtacssSummaries = lens _dtacssSummaries (\s a -> s { _dtacssSummaries = a })
{-# INLINE dtacssSummaries #-}

instance FromJSON DescribeTrustedAdvisorCheckSummariesResponse

instance AWSRequest DescribeTrustedAdvisorCheckSummaries where
    type Sv DescribeTrustedAdvisorCheckSummaries = Support
    type Rs DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummariesResponse

    request = get
    response _ = jsonResponse
