{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the refresh status of the Trusted Advisor checks that have the
-- specified check IDs. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
module Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckRefreshStatuses where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude

data DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsrCheckIds :: [Text]
      -- ^ The IDs of the Trusted Advisor checks.
    } deriving (Show, Generic)

makeLenses ''DescribeTrustedAdvisorCheckRefreshStatuses

instance ToPath DescribeTrustedAdvisorCheckRefreshStatuses

instance ToQuery DescribeTrustedAdvisorCheckRefreshStatuses

instance ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses

instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses

data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrssStatuses :: [TrustedAdvisorCheckRefreshStatus]
      -- ^ The refresh status of the specified Trusted Advisor checks.
    } deriving (Show, Generic)

makeLenses ''DescribeTrustedAdvisorCheckRefreshStatusesResponse

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Sv DescribeTrustedAdvisorCheckRefreshStatuses = Support
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse

    request = get
    response _ = undefined
