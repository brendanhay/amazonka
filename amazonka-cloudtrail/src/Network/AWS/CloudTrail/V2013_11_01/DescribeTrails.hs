{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.DescribeTrails
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves settings for the trail associated with the current region for
-- your account.
module Network.AWS.CloudTrail.V2013_11_01.DescribeTrails where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeTrails' request.
describeTrails :: DescribeTrails
describeTrails = DescribeTrails
    { _dtrTrailNameList = mempty
    }

data DescribeTrails = DescribeTrails
    { _dtrTrailNameList :: [Text]
      -- ^ The trail returned.
    } deriving (Show, Generic)

makeLenses ''DescribeTrails

instance ToPath DescribeTrails

instance ToQuery DescribeTrails

instance ToHeaders DescribeTrails

instance ToJSON DescribeTrails

data DescribeTrailsResponse = DescribeTrailsResponse
    { _dtsTrailList :: [Trail]
      -- ^ The list of trails.
    } deriving (Show, Generic)

makeLenses ''DescribeTrailsResponse

instance FromJSON DescribeTrailsResponse

instance AWSRequest DescribeTrails where
    type Sv DescribeTrails = CloudTrail
    type Rs DescribeTrails = DescribeTrailsResponse

    request = get
    response _ = jsonResponse
