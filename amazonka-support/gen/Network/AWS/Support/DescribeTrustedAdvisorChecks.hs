{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorChecks
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about all available Trusted Advisor checks, including
-- name, ID, category, description, and metadata. You must specify a language
-- code; English ("en") and Japanese ("ja") are currently supported. The
-- response contains a TrustedAdvisorCheckDescription for each check.
module Network.AWS.Support.DescribeTrustedAdvisorChecks
    (
    -- * Request
      DescribeTrustedAdvisorChecks
    -- ** Request constructor
    , describeTrustedAdvisorChecks
    -- ** Request lenses
    , dtacLanguage

    -- * Response
    , DescribeTrustedAdvisorChecksResponse
    -- ** Response constructor
    , describeTrustedAdvisorChecksResponse
    -- ** Response lenses
    , dtacrChecks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Support.Types
import qualified GHC.Exts

newtype DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecks
    { _dtacLanguage :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeTrustedAdvisorChecks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacLanguage' @::@ 'Text'
--
describeTrustedAdvisorChecks :: Text -- ^ 'dtacLanguage'
                             -> DescribeTrustedAdvisorChecks
describeTrustedAdvisorChecks p1 = DescribeTrustedAdvisorChecks
    { _dtacLanguage = p1
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dtacLanguage :: Lens' DescribeTrustedAdvisorChecks Text
dtacLanguage = lens _dtacLanguage (\s a -> s { _dtacLanguage = a })

instance ToPath DescribeTrustedAdvisorChecks where
    toPath = const "/"

instance ToQuery DescribeTrustedAdvisorChecks where
    toQuery = const mempty

instance ToHeaders DescribeTrustedAdvisorChecks

instance ToBody DescribeTrustedAdvisorChecks where
    toBody = toBody . encode . _dtacLanguage

newtype DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse
    { _dtacrChecks :: [TrustedAdvisorCheckDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeTrustedAdvisorChecksResponse where
    type Item DescribeTrustedAdvisorChecksResponse = TrustedAdvisorCheckDescription

    fromList = DescribeTrustedAdvisorChecksResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dtacrChecks

-- | 'DescribeTrustedAdvisorChecksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrChecks' @::@ ['TrustedAdvisorCheckDescription']
--
describeTrustedAdvisorChecksResponse :: DescribeTrustedAdvisorChecksResponse
describeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse
    { _dtacrChecks = mempty
    }

-- | Information about all available Trusted Advisor checks.
dtacrChecks :: Lens' DescribeTrustedAdvisorChecksResponse [TrustedAdvisorCheckDescription]
dtacrChecks = lens _dtacrChecks (\s a -> s { _dtacrChecks = a })

instance AWSRequest DescribeTrustedAdvisorChecks where
    type Sv DescribeTrustedAdvisorChecks = Support
    type Rs DescribeTrustedAdvisorChecks = DescribeTrustedAdvisorChecksResponse

    request  = post
    response = jsonResponse $ \h o -> DescribeTrustedAdvisorChecksResponse
        <$> o .: "checks"
