{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeApplications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of existing applications.
module Network.AWS.ElasticBeanstalk.DescribeApplications
    (
    -- * Request
      DescribeApplications
    -- ** Request constructor
    , describeApplications
    -- ** Request lenses
    , daApplicationNames

    -- * Response
    , DescribeApplicationsResponse
    -- ** Response constructor
    , describeApplicationsResponse
    -- ** Response lenses
    , darApplications
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types

newtype DescribeApplications = DescribeApplications
    { _daApplicationNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList DescribeApplications where
    type Item DescribeApplications = Text

    fromList = DescribeApplications . fromList
    toList   = toList . _daApplicationNames

-- | 'DescribeApplications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daApplicationNames' @::@ ['Text']
--
describeApplications :: DescribeApplications
describeApplications = DescribeApplications
    { _daApplicationNames = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions
-- to only include those with the specified names.
daApplicationNames :: Lens' DescribeApplications [Text]
daApplicationNames =
    lens _daApplicationNames (\s a -> s { _daApplicationNames = a })

instance ToQuery DescribeApplications

instance ToPath DescribeApplications where
    toPath = const "/"

newtype DescribeApplicationsResponse = DescribeApplicationsResponse
    { _darApplications :: [ApplicationDescription]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance IsList DescribeApplicationsResponse where
    type Item DescribeApplicationsResponse = ApplicationDescription

    fromList = DescribeApplicationsResponse . fromList
    toList   = toList . _darApplications

-- | 'DescribeApplicationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darApplications' @::@ ['ApplicationDescription']
--
describeApplicationsResponse :: DescribeApplicationsResponse
describeApplicationsResponse = DescribeApplicationsResponse
    { _darApplications = mempty
    }

-- | This parameter contains a list of ApplicationDescription.
darApplications :: Lens' DescribeApplicationsResponse [ApplicationDescription]
darApplications = lens _darApplications (\s a -> s { _darApplications = a })

instance FromXML DescribeApplicationsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeApplicationsResponse"

instance AWSRequest DescribeApplications where
    type Sv DescribeApplications = ElasticBeanstalk
    type Rs DescribeApplications = DescribeApplicationsResponse

    request  = post "DescribeApplications"
    response = xmlResponse $ \h x -> DescribeApplicationsResponse
        <$> x %| "Applications"
