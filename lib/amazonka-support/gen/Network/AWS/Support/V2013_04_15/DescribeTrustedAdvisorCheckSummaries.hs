{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckSummaries where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries
    { _dtacsrCheckIds :: [Text]
      -- ^ The IDs of the Trusted Advisor checks.
    } deriving (Show, Generic)

makeLenses ''DescribeTrustedAdvisorCheckSummaries

instance ToPath DescribeTrustedAdvisorCheckSummaries

instance ToQuery DescribeTrustedAdvisorCheckSummaries

instance ToHeaders DescribeTrustedAdvisorCheckSummaries

instance ToJSON DescribeTrustedAdvisorCheckSummaries

data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse
    { _dtacssSummaries :: [TrustedAdvisorCheckSummary]
      -- ^ The summary information for the requested Trusted Advisor checks.
    } deriving (Show, Generic)

makeLenses ''DescribeTrustedAdvisorCheckSummariesResponse

instance FromJSON DescribeTrustedAdvisorCheckSummariesResponse

instance AWSRequest DescribeTrustedAdvisorCheckSummaries where
    type Sv DescribeTrustedAdvisorCheckSummaries = Support
    type Rs DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummariesResponse

    request = get
    response _ = jsonResponse
