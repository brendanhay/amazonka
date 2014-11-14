{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the results of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks. The response contains a
-- TrustedAdvisorCheckResult object, which contains these three objects:
-- TrustedAdvisorCategorySpecificSummary TrustedAdvisorResourceDetail
-- TrustedAdvisorResourcesSummary In addition, the response contains these
-- fields: Status. The alert status of the check: "ok" (green), "warning"
-- (yellow), "error" (red), or "not_available". Timestamp. The time of the
-- last refresh of the check. CheckId. The unique identifier for the check.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
    (
    -- * Request
      DescribeTrustedAdvisorCheckResult
    -- ** Request constructor
    , describeTrustedAdvisorCheckResult
    -- ** Request lenses
    , dtacrCheckId
    , dtacrLanguage

    -- * Response
    , DescribeTrustedAdvisorCheckResultResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckResultResponse
    -- ** Response lenses
    , dtacrrResult
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Support.Types

data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult
    { _dtacrCheckId  :: Text
    , _dtacrLanguage :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeTrustedAdvisorCheckResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrCheckId' @::@ 'Text'
--
-- * 'dtacrLanguage' @::@ 'Maybe' 'Text'
--
describeTrustedAdvisorCheckResult :: Text -- ^ 'dtacrCheckId'
                                  -> DescribeTrustedAdvisorCheckResult
describeTrustedAdvisorCheckResult p1 = DescribeTrustedAdvisorCheckResult
    { _dtacrCheckId  = p1
    , _dtacrLanguage = Nothing
    }

-- | The unique identifier for the Trusted Advisor check.
dtacrCheckId :: Lens' DescribeTrustedAdvisorCheckResult Text
dtacrCheckId = lens _dtacrCheckId (\s a -> s { _dtacrCheckId = a })

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dtacrLanguage :: Lens' DescribeTrustedAdvisorCheckResult (Maybe Text)
dtacrLanguage = lens _dtacrLanguage (\s a -> s { _dtacrLanguage = a })

instance ToPath DescribeTrustedAdvisorCheckResult where
    toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckResult where
    toQuery = const mempty

instance ToHeaders DescribeTrustedAdvisorCheckResult

instance ToBody DescribeTrustedAdvisorCheckResult where
    toBody = toBody . encode . _dtacrCheckId

newtype DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse
    { _dtacrrResult :: Maybe TrustedAdvisorCheckResult
    } deriving (Eq, Show, Generic)

-- | 'DescribeTrustedAdvisorCheckResultResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrrResult' @::@ 'Maybe' 'TrustedAdvisorCheckResult'
--
describeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse
describeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse
    { _dtacrrResult = Nothing
    }

-- | The detailed results of the Trusted Advisor check.
dtacrrResult :: Lens' DescribeTrustedAdvisorCheckResultResponse (Maybe TrustedAdvisorCheckResult)
dtacrrResult = lens _dtacrrResult (\s a -> s { _dtacrrResult = a })

instance AWSRequest DescribeTrustedAdvisorCheckResult where
    type Sv DescribeTrustedAdvisorCheckResult = Support
    type Rs DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResultResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeTrustedAdvisorCheckResultResponse
        <$> o .: "result"
