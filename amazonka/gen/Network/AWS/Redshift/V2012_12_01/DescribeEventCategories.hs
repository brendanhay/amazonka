{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.Redshift.V2012_12_01.DescribeEventCategories where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

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
    } deriving (Generic)

instance ToQuery DescribeEventCategories where
    toQuery = genericToQuery def

instance AWSRequest DescribeEventCategories where
    type Sv DescribeEventCategories = Redshift
    type Rs DescribeEventCategories = DescribeEventCategoriesResponse

    request = post "DescribeEventCategories"
    response _ = xmlResponse

data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse
    { _ecrEventCategoriesMapList :: [EventCategoriesMap]
      -- ^ A list of event categories descriptions.
    } deriving (Generic)

instance FromXML DescribeEventCategoriesResponse where
    fromXMLOptions = xmlOptions
