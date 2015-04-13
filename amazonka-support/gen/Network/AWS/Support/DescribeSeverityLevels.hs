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

-- Module      : Network.AWS.Support.DescribeSeverityLevels
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

-- | Returns the list of severity levels that you can assign to an AWS Support
-- case. The severity level for a case is also a field in the 'CaseDetails' data
-- type included in any 'CreateCase' request.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeSeverityLevels.html>
module Network.AWS.Support.DescribeSeverityLevels
    (
    -- * Request
      DescribeSeverityLevels
    -- ** Request constructor
    , describeSeverityLevels
    -- ** Request lenses
    , dslLanguage

    -- * Response
    , DescribeSeverityLevelsResponse
    -- ** Response constructor
    , describeSeverityLevelsResponse
    -- ** Response lenses
    , dslrSeverityLevels
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Support.Types
import qualified GHC.Exts

newtype DescribeSeverityLevels = DescribeSeverityLevels
    { _dslLanguage :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DescribeSeverityLevels' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dslLanguage' @::@ 'Maybe' 'Text'
--
describeSeverityLevels :: DescribeSeverityLevels
describeSeverityLevels = DescribeSeverityLevels
    { _dslLanguage = Nothing
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dslLanguage :: Lens' DescribeSeverityLevels (Maybe Text)
dslLanguage = lens _dslLanguage (\s a -> s { _dslLanguage = a })

newtype DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse
    { _dslrSeverityLevels :: List "severityLevels" SeverityLevel
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeSeverityLevelsResponse where
    type Item DescribeSeverityLevelsResponse = SeverityLevel

    fromList = DescribeSeverityLevelsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dslrSeverityLevels

-- | 'DescribeSeverityLevelsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dslrSeverityLevels' @::@ ['SeverityLevel']
--
describeSeverityLevelsResponse :: DescribeSeverityLevelsResponse
describeSeverityLevelsResponse = DescribeSeverityLevelsResponse
    { _dslrSeverityLevels = mempty
    }

-- | The available severity levels for the support case. Available severity levels
-- are defined by your service level agreement with AWS.
dslrSeverityLevels :: Lens' DescribeSeverityLevelsResponse [SeverityLevel]
dslrSeverityLevels =
    lens _dslrSeverityLevels (\s a -> s { _dslrSeverityLevels = a })
        . _List

instance ToPath DescribeSeverityLevels where
    toPath = const "/"

instance ToQuery DescribeSeverityLevels where
    toQuery = const mempty

instance ToHeaders DescribeSeverityLevels

instance ToJSON DescribeSeverityLevels where
    toJSON DescribeSeverityLevels{..} = object
        [ "language" .= _dslLanguage
        ]

instance AWSRequest DescribeSeverityLevels where
    type Sv DescribeSeverityLevels = Support
    type Rs DescribeSeverityLevels = DescribeSeverityLevelsResponse

    request  = post "DescribeSeverityLevels"
    response = jsonResponse

instance FromJSON DescribeSeverityLevelsResponse where
    parseJSON = withObject "DescribeSeverityLevelsResponse" $ \o -> DescribeSeverityLevelsResponse
        <$> o .:? "severityLevels" .!= mempty
