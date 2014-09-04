{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.Support.V2013_04_15.DescribeTrustedAdvisorCheckRefreshStatuses
    (
    -- * Request
      DescribeTrustedAdvisorCheckRefreshStatuses
    -- ** Request constructor
    , describeTrustedAdvisorCheckRefreshStatuses
    -- ** Request lenses
    , dtacrsrCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckRefreshStatusesResponse
    -- ** Response lenses
    , dtacrssStatuses
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeTrustedAdvisorCheckRefreshStatuses' request.
describeTrustedAdvisorCheckRefreshStatuses :: [Text] -- ^ 'dtacrsrCheckIds'
                                           -> DescribeTrustedAdvisorCheckRefreshStatuses
describeTrustedAdvisorCheckRefreshStatuses p1 = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsrCheckIds = p1
    }
{-# INLINE describeTrustedAdvisorCheckRefreshStatuses #-}

data DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsrCheckIds :: [Text]
      -- ^ The IDs of the Trusted Advisor checks.
    } deriving (Show, Generic)

-- | The IDs of the Trusted Advisor checks.
dtacrsrCheckIds :: Lens' DescribeTrustedAdvisorCheckRefreshStatuses ([Text])
dtacrsrCheckIds f x =
    f (_dtacrsrCheckIds x)
        <&> \y -> x { _dtacrsrCheckIds = y }
{-# INLINE dtacrsrCheckIds #-}

instance ToPath DescribeTrustedAdvisorCheckRefreshStatuses

instance ToQuery DescribeTrustedAdvisorCheckRefreshStatuses

instance ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses

instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses

data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrssStatuses :: [TrustedAdvisorCheckRefreshStatus]
      -- ^ The refresh status of the specified Trusted Advisor checks.
    } deriving (Show, Generic)

-- | The refresh status of the specified Trusted Advisor checks.
dtacrssStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse ([TrustedAdvisorCheckRefreshStatus])
dtacrssStatuses f x =
    f (_dtacrssStatuses x)
        <&> \y -> x { _dtacrssStatuses = y }
{-# INLINE dtacrssStatuses #-}

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Sv DescribeTrustedAdvisorCheckRefreshStatuses = Support
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse

    request = get
    response _ = jsonResponse
