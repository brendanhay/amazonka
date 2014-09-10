{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect
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
module Network.AWS.DirectConnect
    (
    -- * Request
      DescribeInterconnects
    -- ** Request constructor
    , mkDescribeInterconnects
    -- ** Request lenses
    , di1InterconnectId

    -- * Response
    , DescribeInterconnectsResponse
    -- ** Response constructor
    , mkDescribeInterconnectsResponse
    -- ** Response lenses
    , dirrInterconnects
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Container for the parameters to the DescribeInterconnects operation.
newtype DescribeInterconnects = DescribeInterconnects
    { _di1InterconnectId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInterconnects' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InterconnectId ::@ @Maybe Text@
--
mkDescribeInterconnects :: DescribeInterconnects
mkDescribeInterconnects = DescribeInterconnects
    { _di1InterconnectId = Nothing
    }

-- | The ID of the interconnect. Example: dxcon-abc123.
di1InterconnectId :: Lens' DescribeInterconnects (Maybe Text)
di1InterconnectId =
    lens _di1InterconnectId (\s a -> s { _di1InterconnectId = a })

instance ToPath DescribeInterconnects

instance ToQuery DescribeInterconnects

instance ToHeaders DescribeInterconnects

instance ToJSON DescribeInterconnects

-- | A structure containing a list of interconnects.
newtype DescribeInterconnectsResponse = DescribeInterconnectsResponse
    { _dirrInterconnects :: [Interconnect]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeInterconnectsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Interconnects ::@ @[Interconnect]@
--
mkDescribeInterconnectsResponse :: DescribeInterconnectsResponse
mkDescribeInterconnectsResponse = DescribeInterconnectsResponse
    { _dirrInterconnects = mempty
    }

-- | A list of interconnects.
dirrInterconnects :: Lens' DescribeInterconnectsResponse [Interconnect]
dirrInterconnects =
    lens _dirrInterconnects (\s a -> s { _dirrInterconnects = a })

instance FromJSON DescribeInterconnectsResponse

instance AWSRequest DescribeInterconnects where
    type Sv DescribeInterconnects = DirectConnect
    type Rs DescribeInterconnects = DescribeInterconnectsResponse

    request = get
    response _ = jsonResponse
