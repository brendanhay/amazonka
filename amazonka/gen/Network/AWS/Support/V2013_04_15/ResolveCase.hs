{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Support.V2013_04_15.ResolveCase where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.Support.V2013_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ResolveCase' request.
resolveCase :: ResolveCase
resolveCase = ResolveCase
    { _rcrCaseId = Nothing
    }

data ResolveCase = ResolveCase
    { _rcrCaseId :: Maybe Text
      -- ^ The AWS Support case ID requested or returned in the call. The
      -- case ID is an alphanumeric string formatted as shown in this
      -- example: case-12345678910-2013-c4c1d2bf33c5cf47.
    } deriving (Show, Generic)

makeLenses ''ResolveCase

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

makeLenses ''ResolveCaseResponse

instance FromJSON ResolveCaseResponse

instance AWSRequest ResolveCase where
    type Sv ResolveCase = Support
    type Rs ResolveCase = ResolveCaseResponse

    request = get
    response _ = undefined
