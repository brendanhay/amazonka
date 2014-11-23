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
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeApplications.html>
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
import qualified GHC.Exts

newtype DescribeApplications = DescribeApplications
    { _daApplicationNames :: List "ApplicationNames" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeApplications where
    type Item DescribeApplications = Text

    fromList = DescribeApplications . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _daApplicationNames

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
        . _List

newtype DescribeApplicationsResponse = DescribeApplicationsResponse
    { _darApplications :: List "Applications" ApplicationDescription
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeApplicationsResponse where
    type Item DescribeApplicationsResponse = ApplicationDescription

    fromList = DescribeApplicationsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _darApplications

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
darApplications = lens _darApplications (\s a -> s { _darApplications = a }) . _List

instance ToPath DescribeApplications where
    toPath = const "/"

instance ToQuery DescribeApplications where
    toQuery DescribeApplications{..} = mconcat
        [ "ApplicationNames" =? _daApplicationNames
        ]

instance ToHeaders DescribeApplications

instance AWSRequest DescribeApplications where
    type Sv DescribeApplications = ElasticBeanstalk
    type Rs DescribeApplications = DescribeApplicationsResponse

    request  = post "DescribeApplications"
    response = xmlResponse

instance FromXML DescribeApplicationsResponse where
    parseXML = withElement "DescribeApplicationsResult" $ \x -> DescribeApplicationsResponse
        <$> x .@? "Applications"
