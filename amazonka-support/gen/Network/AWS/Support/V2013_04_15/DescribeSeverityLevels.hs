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
    , describeSeverityLevels
    -- ** Request lenses
    , dslrLanguage

    -- * Response
    , DescribeSeverityLevelsResponse
    -- ** Response lenses
    , dslsSeverityLevels
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DescribeSeverityLevels' request.
describeSeverityLevels :: DescribeSeverityLevels
describeSeverityLevels = DescribeSeverityLevels
    { _dslrLanguage = Nothing
    }
{-# INLINE describeSeverityLevels #-}

data DescribeSeverityLevels = DescribeSeverityLevels
    { _dslrLanguage :: Maybe Text
      -- ^ The ISO 639-1 code for the language in which AWS provides
      -- support. AWS Support currently supports English ("en") and
      -- Japanese ("ja"). Language parameters must be passed explicitly
      -- for operations that take them.
    } deriving (Show, Generic)

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English ("en") and Japanese ("ja"). Language
-- parameters must be passed explicitly for operations that take them.
dslrLanguage :: Lens' DescribeSeverityLevels (Maybe Text)
dslrLanguage f x =
    f (_dslrLanguage x)
        <&> \y -> x { _dslrLanguage = y }
{-# INLINE dslrLanguage #-}

instance ToPath DescribeSeverityLevels

instance ToQuery DescribeSeverityLevels

instance ToHeaders DescribeSeverityLevels

instance ToJSON DescribeSeverityLevels

data DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse
    { _dslsSeverityLevels :: [SeverityLevel]
      -- ^ The available severity levels for the support case. Available
      -- severity levels are defined by your service level agreement with
      -- AWS.
    } deriving (Show, Generic)

-- | The available severity levels for the support case. Available severity
-- levels are defined by your service level agreement with AWS.
dslsSeverityLevels :: Lens' DescribeSeverityLevelsResponse ([SeverityLevel])
dslsSeverityLevels f x =
    f (_dslsSeverityLevels x)
        <&> \y -> x { _dslsSeverityLevels = y }
{-# INLINE dslsSeverityLevels #-}

instance FromJSON DescribeSeverityLevelsResponse

instance AWSRequest DescribeSeverityLevels where
    type Sv DescribeSeverityLevels = Support
    type Rs DescribeSeverityLevels = DescribeSeverityLevelsResponse

    request = get
    response _ = jsonResponse
