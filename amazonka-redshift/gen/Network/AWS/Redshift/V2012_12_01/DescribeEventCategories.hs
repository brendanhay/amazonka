{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source types,
-- go to Amazon Redshift Event Notifications.
module Network.AWS.Redshift.V2012_12_01.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategories
    -- ** Request constructor
    , mkDescribeEventCategoriesMessage
    -- ** Request lenses
    , decmSourceType

    -- * Response
    , DescribeEventCategoriesResponse
    -- ** Response lenses
    , ecnEventCategoriesMapList
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventCategories' request.
mkDescribeEventCategoriesMessage :: DescribeEventCategories
mkDescribeEventCategoriesMessage = DescribeEventCategories
    { _decmSourceType = Nothing
    }
{-# INLINE mkDescribeEventCategoriesMessage #-}

newtype DescribeEventCategories = DescribeEventCategories
    { _decmSourceType :: Maybe Text
      -- ^ The source type, such as cluster or parameter group, to which the
      -- described event categories apply. Valid values: cluster,
      -- snapshot, parameter group, and security group.
    } deriving (Show, Generic)

-- | The source type, such as cluster or parameter group, to which the described
-- event categories apply. Valid values: cluster, snapshot, parameter group,
-- and security group.
decmSourceType :: Lens' DescribeEventCategories (Maybe Text)
decmSourceType = lens _decmSourceType (\s a -> s { _decmSourceType = a })
{-# INLINE decmSourceType #-}

instance ToQuery DescribeEventCategories where
    toQuery = genericQuery def

newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _ecnEventCategoriesMapList :: [EventCategoriesMap]
      -- ^ A list of event categories descriptions.
    } deriving (Show, Generic)

-- | A list of event categories descriptions.
ecnEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse ([EventCategoriesMap])
ecnEventCategoriesMapList = lens _ecnEventCategoriesMapList (\s a -> s { _ecnEventCategoriesMapList = a })
{-# INLINE ecnEventCategoriesMapList #-}

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = Redshift
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request = post "DescribeEventCategories"
    response _ = xmlResponse
