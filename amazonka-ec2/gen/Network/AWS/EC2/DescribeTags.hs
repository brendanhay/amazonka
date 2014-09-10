{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more of the tags for your EC2 resources. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide. Example This example describes all the tags in
-- your account. https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE ami-1a2b3c4d image
-- webserver ami-1a2b3c4d image stack Production i-5f4e3d2a instance webserver
-- i-5f4e3d2a instance stack Production i-12345678 instance database_server
-- i-12345678 instance stack Test Example This example describes only the tags
-- for the AMI with ID ami-1a2b3c4d.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-id &amp;Filter.1.Value.1=ami-1a2b3c4d
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE ami-1a2b3c4d image
-- webserver ami-1a2b3c4d image stack Production Example This example
-- describes the tags for all your instances.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance
-- webserver i-5f4e3d2a instance stack Production i-12345678 instance
-- database_server i-12345678 instance stack Test Example This example
-- describes the tags for all your instances tagged with the key webserver.
-- Note that you can use wildcards with filters, so you could specify the
-- value as ?ebserver to find tags with the key webserver or Webserver.
-- https://ec2.amazonaws.com/?Action=DescribeTags &amp;Filter.1.Name=key
-- &amp;Filter.1.Value.1=webserver &amp;AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance webserver Example
-- This example describes the tags for all your instances tagged with either
-- stack=Test or stack=Production.
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;Filter.2.Name=key &amp;Filter.2.Value.1=stack &amp;Filter.3.Name=value
-- &amp;Filter.3.Value.1=Test &amp;Filter.3.Value.2=Production &amp;AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE i-5f4e3d2a instance stack Production
-- i-12345678 instance stack Test Example This example describes the tags for
-- all your instances tagged with Purpose=[empty string].
-- https://ec2.amazonaws.com/?Action=DescribeTags
-- &amp;Filter.1.Name=resource-type &amp;Filter.1.Value.1=instance
-- &amp;Filter.2.Name=key &amp;Filter.2.Value.1=Purpose
-- &amp;Filter.3.Name=value &amp;Filter.3.Value.1= &amp;AUTHPARAMS.
module Network.AWS.EC2
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , mkDescribeTags
    -- ** Request lenses
    , dt1Filters
    , dt1MaxResults
    , dt1NextToken

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , mkDescribeTagsResponse
    -- ** Response lenses
    , dtrTags
    , dtrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeTags = DescribeTags
    { _dt1Filters :: [Filter]
    , _dt1MaxResults :: Maybe Integer
    , _dt1NextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTags' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Filters ::@ @[Filter]@
--
-- * @MaxResults ::@ @Maybe Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeTags :: DescribeTags
mkDescribeTags = DescribeTags
    { _dt1Filters = mempty
    , _dt1MaxResults = Nothing
    , _dt1NextToken = Nothing
    }

-- | One or more filters. key - The tag key. resource-id - The resource ID.
-- resource-type - The resource type (customer-gateway | dhcp-options | image
-- | instance | internet-gateway | network-acl | network-interface |
-- reserved-instances | route-table | security-group | snapshot |
-- spot-instances-request | subnet | volume | vpc | vpn-connection |
-- vpn-gateway). value - The tag value.
dt1Filters :: Lens' DescribeTags [Filter]
dt1Filters = lens _dt1Filters (\s a -> s { _dt1Filters = a })

-- | The maximum number of items to return for this call. The call also returns
-- a token that you can specify in a subsequent call to get the next set of
-- results. If the value is greater than 1000, we return only 1000 items.
dt1MaxResults :: Lens' DescribeTags (Maybe Integer)
dt1MaxResults = lens _dt1MaxResults (\s a -> s { _dt1MaxResults = a })

-- | The token for the next set of items to return. (You received this token
-- from a prior call.).
dt1NextToken :: Lens' DescribeTags (Maybe Text)
dt1NextToken = lens _dt1NextToken (\s a -> s { _dt1NextToken = a })

instance ToQuery DescribeTags where
    toQuery = genericQuery def

data DescribeTagsResponse = DescribeTagsResponse
    { _dtrTags :: [TagDescription]
    , _dtrNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeTagsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Tags ::@ @[TagDescription]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeTagsResponse :: DescribeTagsResponse
mkDescribeTagsResponse = DescribeTagsResponse
    { _dtrTags = mempty
    , _dtrNextToken = Nothing
    }

-- | A list of tags.
dtrTags :: Lens' DescribeTagsResponse [TagDescription]
dtrTags = lens _dtrTags (\s a -> s { _dtrTags = a })

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dtrNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrNextToken = lens _dtrNextToken (\s a -> s { _dtrNextToken = a })

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeTags where
    type Sv DescribeTags = EC2
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse

instance AWSPager DescribeTags where
    next rq rs = (\x -> rq & dt1NextToken ?~ x)
        <$> (rs ^. dtrNextToken)
