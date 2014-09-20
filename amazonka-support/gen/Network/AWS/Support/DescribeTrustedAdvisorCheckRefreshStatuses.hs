{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
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
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
    (
    -- * Request
      DescribeTrustedAdvisorCheckRefreshStatuses
    -- ** Request constructor
    , describeTrustedAdvisorCheckRefreshStatuses
    -- ** Request lenses
    , dtacrsCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckRefreshStatusesResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckRefreshStatusesResponse
    -- ** Response lenses
    , dtacrsrStatuses
    ) where

import Network.AWS.Support.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsCheckIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTrustedAdvisorCheckRefreshStatuses' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CheckIds ::@ @[Text]@
--
describeTrustedAdvisorCheckRefreshStatuses :: [Text] -- ^ 'dtacrsCheckIds'
                                           -> DescribeTrustedAdvisorCheckRefreshStatuses
describeTrustedAdvisorCheckRefreshStatuses p1 = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsCheckIds = p1
    }

-- | The IDs of the Trusted Advisor checks.
dtacrsCheckIds :: Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Text]
dtacrsCheckIds = lens _dtacrsCheckIds (\s a -> s { _dtacrsCheckIds = a })

instance ToPath DescribeTrustedAdvisorCheckRefreshStatuses

instance ToQuery DescribeTrustedAdvisorCheckRefreshStatuses

instance ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses

instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses

-- | The statuses of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckRefreshStatuses operation.
newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrsrStatuses :: [TrustedAdvisorCheckRefreshStatus]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Statuses ::@ @[TrustedAdvisorCheckRefreshStatus]@
--
describeTrustedAdvisorCheckRefreshStatusesResponse :: [TrustedAdvisorCheckRefreshStatus] -- ^ 'dtacrsrStatuses'
                                                   -> DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatusesResponse p1 = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrsrStatuses = p1
    }

-- | The refresh status of the specified Trusted Advisor checks.
dtacrsrStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrStatuses = lens _dtacrsrStatuses (\s a -> s { _dtacrsrStatuses = a })

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Sv DescribeTrustedAdvisorCheckRefreshStatuses = Support
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse

    request = get
    response _ = jsonResponse
