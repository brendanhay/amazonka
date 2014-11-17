{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorCheckRefreshStatuses.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

newtype DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsCheckIds :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTrustedAdvisorCheckRefreshStatuses where
    type Item DescribeTrustedAdvisorCheckRefreshStatuses = Text

    fromList = DescribeTrustedAdvisorCheckRefreshStatuses . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtacrsCheckIds

-- | 'DescribeTrustedAdvisorCheckRefreshStatuses' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrsCheckIds' @::@ ['Text']
--
describeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses
describeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses
    { _dtacrsCheckIds = mempty
    }

-- | The IDs of the Trusted Advisor checks.
dtacrsCheckIds :: Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Text]
dtacrsCheckIds = lens _dtacrsCheckIds (\s a -> s { _dtacrsCheckIds = a })

newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrsrStatuses :: [TrustedAdvisorCheckRefreshStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTrustedAdvisorCheckRefreshStatusesResponse where
    type Item DescribeTrustedAdvisorCheckRefreshStatusesResponse = TrustedAdvisorCheckRefreshStatus

    fromList = DescribeTrustedAdvisorCheckRefreshStatusesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtacrsrStatuses

-- | 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrsrStatuses' @::@ ['TrustedAdvisorCheckRefreshStatus']
--
describeTrustedAdvisorCheckRefreshStatusesResponse :: DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrsrStatuses = mempty
    }

-- | The refresh status of the specified Trusted Advisor checks.
dtacrsrStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrStatuses = lens _dtacrsrStatuses (\s a -> s { _dtacrsrStatuses = a })

instance ToPath DescribeTrustedAdvisorCheckRefreshStatuses where
    toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckRefreshStatuses where
    toQuery = const mempty

instance ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses
instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Sv DescribeTrustedAdvisorCheckRefreshStatuses = Support
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse

    request  = post
    response = jsonResponse

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse where
    parseJSON = genericParseJSON jsonOptions
