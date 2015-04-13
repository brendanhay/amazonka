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

-- Module      : Network.AWS.DirectConnect.DescribeInterconnects
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

-- | Returns a list of interconnects owned by the AWS account.
--
-- If an interconnect ID is provided, it will only return this particular
-- interconnect.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_DescribeInterconnects.html>
module Network.AWS.DirectConnect.DescribeInterconnects
    (
    -- * Request
      DescribeInterconnects
    -- ** Request constructor
    , describeInterconnects
    -- ** Request lenses
    , diInterconnectId

    -- * Response
    , DescribeInterconnectsResponse
    -- ** Response constructor
    , describeInterconnectsResponse
    -- ** Response lenses
    , dirInterconnects
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.Types
import qualified GHC.Exts

newtype DescribeInterconnects = DescribeInterconnects
    { _diInterconnectId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'DescribeInterconnects' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diInterconnectId' @::@ 'Maybe' 'Text'
--
describeInterconnects :: DescribeInterconnects
describeInterconnects = DescribeInterconnects
    { _diInterconnectId = Nothing
    }

diInterconnectId :: Lens' DescribeInterconnects (Maybe Text)
diInterconnectId = lens _diInterconnectId (\s a -> s { _diInterconnectId = a })

newtype DescribeInterconnectsResponse = DescribeInterconnectsResponse
    { _dirInterconnects :: List "interconnects" Interconnect
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeInterconnectsResponse where
    type Item DescribeInterconnectsResponse = Interconnect

    fromList = DescribeInterconnectsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dirInterconnects

-- | 'DescribeInterconnectsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirInterconnects' @::@ ['Interconnect']
--
describeInterconnectsResponse :: DescribeInterconnectsResponse
describeInterconnectsResponse = DescribeInterconnectsResponse
    { _dirInterconnects = mempty
    }

-- | A list of interconnects.
dirInterconnects :: Lens' DescribeInterconnectsResponse [Interconnect]
dirInterconnects = lens _dirInterconnects (\s a -> s { _dirInterconnects = a }) . _List

instance ToPath DescribeInterconnects where
    toPath = const "/"

instance ToQuery DescribeInterconnects where
    toQuery = const mempty

instance ToHeaders DescribeInterconnects

instance ToJSON DescribeInterconnects where
    toJSON DescribeInterconnects{..} = object
        [ "interconnectId" .= _diInterconnectId
        ]

instance AWSRequest DescribeInterconnects where
    type Sv DescribeInterconnects = DirectConnect
    type Rs DescribeInterconnects = DescribeInterconnectsResponse

    request  = post "DescribeInterconnects"
    response = jsonResponse

instance FromJSON DescribeInterconnectsResponse where
    parseJSON = withObject "DescribeInterconnectsResponse" $ \o -> DescribeInterconnectsResponse
        <$> o .:? "interconnects" .!= mempty
