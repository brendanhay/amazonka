{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.Redshift.V2012_12_01.DescribeEventCategories where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeEventCategories' request.
describeEventCategories :: DescribeEventCategories
describeEventCategories = DescribeEventCategories
    { _decmSourceType = Nothing
    }

data DescribeEventCategories = DescribeEventCategories
    { _decmSourceType :: Maybe Text
      -- ^ The source type, such as cluster or parameter group, to which the
      -- described event categories apply. Valid values: cluster,
      -- snapshot, parameter group, and security group.
    } deriving (Show, Generic)

makeLenses ''DescribeEventCategories

instance ToQuery DescribeEventCategories where
    toQuery = genericQuery def

data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _ecrEventCategoriesMapList :: [EventCategoriesMap]
      -- ^ A list of event categories descriptions.
    } deriving (Show, Generic)

makeLenses ''DescribeEventCategoriesResponse

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = Redshift
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request = post "DescribeEventCategories"
    response _ = xmlResponse
