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

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the refresh status of the Trusted Advisor checks that have the
-- specified check IDs. Check IDs can be obtained by calling 'DescribeTrustedAdvisorChecks'.
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
    { _dtacrsCheckIds :: List "checkIds" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

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
dtacrsCheckIds = lens _dtacrsCheckIds (\s a -> s { _dtacrsCheckIds = a }) . _List

newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse
    { _dtacrsrStatuses :: List "statuses" TrustedAdvisorCheckRefreshStatus
    } deriving (Eq, Show, Monoid, Semigroup)

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
dtacrsrStatuses = lens _dtacrsrStatuses (\s a -> s { _dtacrsrStatuses = a }) . _List

instance ToPath DescribeTrustedAdvisorCheckRefreshStatuses where
    toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckRefreshStatuses where
    toQuery = const mempty

instance ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses

instance ToJSON DescribeTrustedAdvisorCheckRefreshStatuses where
    toJSON DescribeTrustedAdvisorCheckRefreshStatuses{..} = object
        [ "checkIds" .= _dtacrsCheckIds
        ]

instance AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
    type Sv DescribeTrustedAdvisorCheckRefreshStatuses = Support
    type Rs DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatusesResponse

    request  = post "DescribeTrustedAdvisorCheckRefreshStatuses"
    response = jsonResponse

instance FromJSON DescribeTrustedAdvisorCheckRefreshStatusesResponse where
    parseJSON = withObject "DescribeTrustedAdvisorCheckRefreshStatusesResponse" $ \o -> DescribeTrustedAdvisorCheckRefreshStatusesResponse
        <$> o .:? "statuses" .!= mempty
