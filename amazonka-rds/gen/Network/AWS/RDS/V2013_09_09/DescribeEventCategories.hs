{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DescribeEventCategories
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays a list of categories for all event source types, or, if specified,
-- for a specified source type. You can see a list of the event categories and
-- source types in the Events topic in the Amazon RDS User Guide.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventCategories
-- &SourceType=db-instance &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T013452Z &AWSAccessKeyId=
-- &Signature= db-instance failover low storage maintenance recovery
-- restoration deletion configuration change failover availability creation
-- backup notification ea3bf54b-68ea-11e2-bd13-a92da73b3119.
module Network.AWS.RDS.V2013_09_09.DescribeEventCategories
    (
    -- * Request
      DescribeEventCategories
    -- ** Request constructor
    , mkDescribeEventCategories
    -- ** Request lenses
    , decSourceType

    -- * Response
    , DescribeEventCategoriesResponse
    -- ** Response lenses
    , decrsEventCategoriesMapList
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
newtype DescribeEventCategories = DescribeEventCategories
    { _decSourceType :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeEventCategories' request.
mkDescribeEventCategories :: DescribeEventCategories
mkDescribeEventCategories = DescribeEventCategories
    { _decSourceType = Nothing
    }
{-# INLINE mkDescribeEventCategories #-}

-- | The type of source that will be generating the events. Valid values:
-- db-instance | db-parameter-group | db-security-group | db-snapshot.
decSourceType :: Lens' DescribeEventCategories (Maybe Text)
decSourceType = lens _decSourceType (\s a -> s { _decSourceType = a })
{-# INLINE decSourceType #-}

instance ToQuery DescribeEventCategories where
    toQuery = genericQuery def

-- | Data returned from the DescribeEventCategories action.
newtype DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _decrsEventCategoriesMapList :: [EventCategoriesMap]
    } deriving (Show, Generic)

-- | A list of EventCategoriesMap data types.
decrsEventCategoriesMapList :: Lens' DescribeEventCategoriesResponse [EventCategoriesMap]
decrsEventCategoriesMapList =
    lens _decrsEventCategoriesMapList
         (\s a -> s { _decrsEventCategoriesMapList = a })
{-# INLINE decrsEventCategoriesMapList #-}

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = RDS
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request = post "DescribeEventCategories"
    response _ = xmlResponse
