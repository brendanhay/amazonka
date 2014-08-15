{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeInterconnects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of interconnects owned by the AWS account. If an
-- interconnect ID is provided, it will only return this particular
-- interconnect.
module Network.AWS.DirectConnect.V2012_10_25.DescribeInterconnects where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'DescribeInterconnects' request.
describeInterconnects :: DescribeInterconnects
describeInterconnects = DescribeInterconnects
    { _dirInterconnectId = Nothing
    }

data DescribeInterconnects = DescribeInterconnects
    { _dirInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    } deriving (Show, Generic)

makeLenses ''DescribeInterconnects

instance ToPath DescribeInterconnects

instance ToQuery DescribeInterconnects

instance ToHeaders DescribeInterconnects

instance ToJSON DescribeInterconnects

data DescribeInterconnectsResponse = DescribeInterconnectsResponse
    { _iInterconnects :: [Interconnect]
      -- ^ A list of interconnects.
    } deriving (Show, Generic)

makeLenses ''DescribeInterconnectsResponse

instance FromJSON DescribeInterconnectsResponse

instance AWSRequest DescribeInterconnects where
    type Sv DescribeInterconnects = DirectConnect
    type Rs DescribeInterconnects = DescribeInterconnectsResponse

    request = get
    response _ = jsonResponse
