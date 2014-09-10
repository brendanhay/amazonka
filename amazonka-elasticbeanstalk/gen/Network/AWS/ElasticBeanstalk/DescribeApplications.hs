{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticBeanstalk
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of existing applications.
-- https://elasticbeanstalk.us-east-1.amazon.com/?ApplicationNames.member.1=SampleApplication
-- &Operation=DescribeApplications &AuthParams Sample Description
-- SampleApplication 2010-11-16T20:20:51.974Z 2010-11-16T20:20:51.974Z Default
-- 577c70ff-f1d7-11df-8a78-9f77047e0d0c.
module Network.AWS.ElasticBeanstalk
    (
    -- * Request
      DescribeApplications
    -- ** Request constructor
    , mkDescribeApplications
    -- ** Request lenses
    , da1ApplicationNames

    -- * Response
    , DescribeApplicationsResponse
    -- ** Response constructor
    , mkDescribeApplicationsResponse
    -- ** Response lenses
    , darApplications
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude

-- | This documentation target is not reported in the API reference.
newtype DescribeApplications = DescribeApplications
    { _da1ApplicationNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApplications' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ApplicationNames ::@ @[Text]@
--
mkDescribeApplications :: DescribeApplications
mkDescribeApplications = DescribeApplications
    { _da1ApplicationNames = mempty
    }

-- | If specified, AWS Elastic Beanstalk restricts the returned descriptions to
-- only include those with the specified names.
da1ApplicationNames :: Lens' DescribeApplications [Text]
da1ApplicationNames =
    lens _da1ApplicationNames (\s a -> s { _da1ApplicationNames = a })

instance ToQuery DescribeApplications where
    toQuery = genericQuery def

-- | Result message containing a list of application descriptions.
newtype DescribeApplicationsResponse = DescribeApplicationsResponse
    { _darApplications :: [ApplicationDescription]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeApplicationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Applications ::@ @[ApplicationDescription]@
--
mkDescribeApplicationsResponse :: DescribeApplicationsResponse
mkDescribeApplicationsResponse = DescribeApplicationsResponse
    { _darApplications = mempty
    }

-- | This parameter contains a list of ApplicationDescription.
darApplications :: Lens' DescribeApplicationsResponse [ApplicationDescription]
darApplications = lens _darApplications (\s a -> s { _darApplications = a })

instance FromXML DescribeApplicationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeApplications where
    type Sv DescribeApplications = ElasticBeanstalk
    type Rs DescribeApplications = DescribeApplicationsResponse

    request = post "DescribeApplications"
    response _ = xmlResponse
