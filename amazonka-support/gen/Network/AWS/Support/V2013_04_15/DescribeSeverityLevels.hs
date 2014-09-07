{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.DescribeSeverityLevels
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the list of severity levels that you can assign to an AWS Support
-- case. The severity level for a case is also a field in the CaseDetails data
-- type included in any CreateCase request.
module Network.AWS.Support.V2013_04_15.DescribeSeverityLevels
    (
    -- * Request
      DescribeSeverityLevels
    -- ** Request constructor
    , mkDescribeSeverityLevels
    -- ** Request lenses
    , dslLanguage

    -- * Response
    , DescribeSeverityLevelsResponse
    -- ** Response lenses
    , dslrsSeverityLevels
    ) where

import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | 
newtype DescribeSeverityLevels = DescribeSeverityLevels
    { _dslLanguage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSeverityLevels' request.
mkDescribeSeverityLevels :: DescribeSeverityLevels
mkDescribeSeverityLevels = DescribeSeverityLevels
    { _dslLanguage = Nothing
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dslLanguage :: Lens' DescribeSeverityLevels (Maybe Text)
dslLanguage = lens _dslLanguage (\s a -> s { _dslLanguage = a })

instance ToPath DescribeSeverityLevels

instance ToQuery DescribeSeverityLevels

instance ToHeaders DescribeSeverityLevels

instance ToJSON DescribeSeverityLevels

-- | The list of severity levels returned by the DescribeSeverityLevels
-- operation.
newtype DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse
    { _dslrsSeverityLevels :: [SeverityLevel]
    } deriving (Show, Generic)

-- | The available severity levels for the support case. Available severity
-- levels are defined by your service level agreement with AWS.
dslrsSeverityLevels :: Lens' DescribeSeverityLevelsResponse [SeverityLevel]
dslrsSeverityLevels =
    lens _dslrsSeverityLevels (\s a -> s { _dslrsSeverityLevels = a })

instance FromJSON DescribeSeverityLevelsResponse

instance AWSRequest DescribeSeverityLevels where
    type Sv DescribeSeverityLevels = Support
    type Rs DescribeSeverityLevels = DescribeSeverityLevelsResponse

    request = get
    response _ = jsonResponse
