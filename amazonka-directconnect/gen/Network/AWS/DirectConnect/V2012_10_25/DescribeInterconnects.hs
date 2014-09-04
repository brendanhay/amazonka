{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.DirectConnect.V2012_10_25.DescribeInterconnects
    (
    -- * Request
      DescribeInterconnects
    -- ** Request constructor
    , mkDescribeInterconnectsRequest
    -- ** Request lenses
    , ditInterconnectId

    -- * Response
    , DescribeInterconnectsResponse
    -- ** Response lenses
    , yInterconnects
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInterconnects' request.
mkDescribeInterconnectsRequest :: DescribeInterconnects
mkDescribeInterconnectsRequest = DescribeInterconnects
    { _ditInterconnectId = Nothing
    }
{-# INLINE mkDescribeInterconnectsRequest #-}

newtype DescribeInterconnects = DescribeInterconnects
    { _ditInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    } deriving (Show, Generic)

-- | The ID of the interconnect. Example: dxcon-abc123.
ditInterconnectId :: Lens' DescribeInterconnects (Maybe Text)
ditInterconnectId = lens _ditInterconnectId (\s a -> s { _ditInterconnectId = a })
{-# INLINE ditInterconnectId #-}

instance ToPath DescribeInterconnects

instance ToQuery DescribeInterconnects

instance ToHeaders DescribeInterconnects

instance ToJSON DescribeInterconnects

newtype DescribeInterconnectsResponse = DescribeInterconnectsResponse
    { _yInterconnects :: [Interconnect]
      -- ^ A list of interconnects.
    } deriving (Show, Generic)

-- | A list of interconnects.
yInterconnects :: Lens' DescribeInterconnectsResponse ([Interconnect])
yInterconnects = lens _yInterconnects (\s a -> s { _yInterconnects = a })
{-# INLINE yInterconnects #-}

instance FromJSON DescribeInterconnectsResponse

instance AWSRequest DescribeInterconnects where
    type Sv DescribeInterconnects = DirectConnect
    type Rs DescribeInterconnects = DescribeInterconnectsResponse

    request = get
    response _ = jsonResponse
