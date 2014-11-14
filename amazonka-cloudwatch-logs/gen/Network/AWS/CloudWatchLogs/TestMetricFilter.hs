{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudWatchLogs.TestMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Tests the filter pattern of a metric filter against a sample of log event
-- messages. You can use this operation to validate the correctness of a
-- metric filter pattern.
module Network.AWS.CloudWatchLogs.TestMetricFilter
    (
    -- * Request
      TestMetricFilter
    -- ** Request constructor
    , testMetricFilter
    -- ** Request lenses
    , tmfFilterPattern
    , tmfLogEventMessages

    -- * Response
    , TestMetricFilterResponse
    -- ** Response constructor
    , testMetricFilterResponse
    -- ** Response lenses
    , tmfrMatches
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data TestMetricFilter = TestMetricFilter
    { _tmfFilterPattern    :: Text
    , _tmfLogEventMessages :: List1 Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'TestMetricFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tmfFilterPattern' @::@ 'Text'
--
-- * 'tmfLogEventMessages' @::@ 'NonEmpty' 'Text'
--
testMetricFilter :: Text -- ^ 'tmfFilterPattern'
                 -> NonEmpty Text -- ^ 'tmfLogEventMessages'
                 -> TestMetricFilter
testMetricFilter p1 p2 = TestMetricFilter
    { _tmfFilterPattern    = p1
    , _tmfLogEventMessages = withIso _List1 (const id) p2
    }

tmfFilterPattern :: Lens' TestMetricFilter Text
tmfFilterPattern = lens _tmfFilterPattern (\s a -> s { _tmfFilterPattern = a })

tmfLogEventMessages :: Lens' TestMetricFilter (NonEmpty Text)
tmfLogEventMessages =
    lens _tmfLogEventMessages (\s a -> s { _tmfLogEventMessages = a })
        . _List1

instance ToPath TestMetricFilter where
    toPath = const "/"

instance ToQuery TestMetricFilter where
    toQuery = const mempty

instance ToHeaders TestMetricFilter

instance ToBody TestMetricFilter where
    toBody = toBody . encode . _tmfFilterPattern

newtype TestMetricFilterResponse = TestMetricFilterResponse
    { _tmfrMatches :: [MetricFilterMatchRecord]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList TestMetricFilterResponse where
    type Item TestMetricFilterResponse = MetricFilterMatchRecord

    fromList = TestMetricFilterResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _tmfrMatches

-- | 'TestMetricFilterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tmfrMatches' @::@ ['MetricFilterMatchRecord']
--
testMetricFilterResponse :: TestMetricFilterResponse
testMetricFilterResponse = TestMetricFilterResponse
    { _tmfrMatches = mempty
    }

tmfrMatches :: Lens' TestMetricFilterResponse [MetricFilterMatchRecord]
tmfrMatches = lens _tmfrMatches (\s a -> s { _tmfrMatches = a })

instance AWSRequest TestMetricFilter where
    type Sv TestMetricFilter = CloudWatchLogs
    type Rs TestMetricFilter = TestMetricFilterResponse

    request  = post
    response = jsonResponse $ \h o -> TestMetricFilterResponse
        <$> o .: "matches"
