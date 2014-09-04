{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Support.V2013_04_15.ResolveCase
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Takes a CaseId and returns the initial state of the case along with the
-- state of the case after the call to ResolveCase completed.
module Network.AWS.Support.V2013_04_15.ResolveCase
    (
    -- * Request
      ResolveCase
    -- ** Request constructor
    , resolveCase
    -- ** Request lenses
    , rcrCaseId

    -- * Response
    , ResolveCaseResponse
    -- ** Response lenses
    , rcsInitialCaseStatus
    , rcsFinalCaseStatus
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ResolveCase' request.
resolveCase :: ResolveCase
resolveCase = ResolveCase
    { _rcrCaseId = Nothing
    }
{-# INLINE resolveCase #-}

data ResolveCase = ResolveCase
    { _rcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    } deriving (Show, Generic)

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
rcrCaseId :: Lens' ResolveCase (Maybe Text)
rcrCaseId f x =
    f (_rcrCaseId x)
        <&> \y -> x { _rcrCaseId = y }
{-# INLINE rcrCaseId #-}

instance ToPath ResolveCase

instance ToQuery ResolveCase

instance ToHeaders ResolveCase

instance ToJSON ResolveCase

data ResolveCaseResponse = ResolveCaseResponse
    { _rcsInitialCaseStatus :: Maybe Text
      -- ^ The status of the case when the ResolveCase request was sent.
    , _rcsFinalCaseStatus :: Maybe Text
      -- ^ The status of the case after the ResolveCase request was
      -- processed.
    } deriving (Show, Generic)

-- | The status of the case when the ResolveCase request was sent.
rcsInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcsInitialCaseStatus f x =
    f (_rcsInitialCaseStatus x)
        <&> \y -> x { _rcsInitialCaseStatus = y }
{-# INLINE rcsInitialCaseStatus #-}

-- | The status of the case after the ResolveCase request was processed.
rcsFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcsFinalCaseStatus f x =
    f (_rcsFinalCaseStatus x)
        <&> \y -> x { _rcsFinalCaseStatus = y }
{-# INLINE rcsFinalCaseStatus #-}

instance FromJSON ResolveCaseResponse

instance AWSRequest ResolveCase where
    type Sv ResolveCase = Support
    type Rs ResolveCase = ResolveCaseResponse

    request = get
    response _ = jsonResponse
