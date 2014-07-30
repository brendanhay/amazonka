{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeTags
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
module Network.AWS.EC2.V2014_06_15.DescribeTags where

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
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeTags' request.
describeTags :: DescribeTags
describeTags = DescribeTags
    { _dtrDryRun = Nothing
    , _dtrFilters = mempty
    , _dtrMaxResults = Nothing
    , _dtrNextToken = Nothing
    }

data DescribeTags = DescribeTags
    { _dtrDryRun :: Maybe Bool
      -- ^ 
    , _dtrFilters :: [Filter]
      -- ^ One or more filters. key - The tag key. resource-id - The
      -- resource ID. resource-type - The resource type (customer-gateway
      -- | dhcp-options | image | instance | internet-gateway |
      -- network-acl | network-interface | reserved-instances |
      -- route-table | security-group | snapshot | spot-instances-request
      -- | subnet | volume | vpc | vpn-connection | vpn-gateway). value -
      -- The tag value.
    , _dtrMaxResults :: Maybe Integer
      -- ^ The maximum number of items to return for this call. The call
      -- also returns a token that you can specify in a subsequent call to
      -- get the next set of results. If the value is greater than 1000,
      -- we return only 1000 items.
    , _dtrNextToken :: Maybe Text
      -- ^ The token for the next set of items to return. (You received this
      -- token from a prior call.).
    } deriving (Generic)

instance ToQuery DescribeTags where
    toQuery = genericToQuery def

instance AWSRequest DescribeTags where
    type Sv DescribeTags = EC2
    type Rs DescribeTags = DescribeTagsResponse

    request = post "DescribeTags"
    response _ = xmlResponse

instance AWSPager DescribeTags where
    next rq rs = (\x -> rq { _dtrNextToken = Just x })
        <$> _dtsNextToken rs

data DescribeTagsResponse = DescribeTagsResponse
    { _dtsTags :: [TagDescription]
      -- ^ A list of tags.
    , _dtsNextToken :: Maybe Text
      -- ^ The token to use when requesting the next set of items. If there
      -- are no additional items to return, the string is empty.
    } deriving (Generic)

instance FromXML DescribeTagsResponse where
    fromXMLOptions = xmlOptions
