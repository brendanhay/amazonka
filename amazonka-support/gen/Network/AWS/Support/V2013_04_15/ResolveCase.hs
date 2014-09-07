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
    , mkResolveCase
    -- ** Request lenses
    , rcCaseId

    -- * Response
    , ResolveCaseResponse
    -- ** Response lenses
    , rcrsInitialCaseStatus
    , rcrsFinalCaseStatus
    ) where

import           Network.AWS.Support.V2013_04_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | 
newtype ResolveCase = ResolveCase
    { _rcCaseId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResolveCase' request.
mkResolveCase :: ResolveCase
mkResolveCase = ResolveCase
    { _rcCaseId = Nothing
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is
-- an alphanumeric string formatted as shown in this example:
-- case-12345678910-2013-c4c1d2bf33c5cf47.
rcCaseId :: Lens' ResolveCase (Maybe Text)
rcCaseId = lens _rcCaseId (\s a -> s { _rcCaseId = a })

instance ToPath ResolveCase

instance ToQuery ResolveCase

instance ToHeaders ResolveCase

instance ToJSON ResolveCase

-- | The status of the case returned by the ResolveCase operation.
data ResolveCaseResponse = ResolveCaseResponse
    { _rcrsInitialCaseStatus :: Maybe Text
    , _rcrsFinalCaseStatus :: Maybe Text
    } deriving (Show, Generic)

-- | The status of the case when the ResolveCase request was sent.
rcrsInitialCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsInitialCaseStatus =
    lens _rcrsInitialCaseStatus (\s a -> s { _rcrsInitialCaseStatus = a })

-- | The status of the case after the ResolveCase request was processed.
rcrsFinalCaseStatus :: Lens' ResolveCaseResponse (Maybe Text)
rcrsFinalCaseStatus =
    lens _rcrsFinalCaseStatus (\s a -> s { _rcrsFinalCaseStatus = a })

instance FromJSON ResolveCaseResponse

instance AWSRequest ResolveCase where
    type Sv ResolveCase = Support
    type Rs ResolveCase = ResolveCaseResponse

    request = get
    response _ = jsonResponse
