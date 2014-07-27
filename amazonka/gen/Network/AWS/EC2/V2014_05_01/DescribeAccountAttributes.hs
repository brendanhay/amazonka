{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DescribeAccountAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of your AWS account. Example This example
-- describes the platforms that are supported by your AWS account. The first
-- response is for an account that supports only EC2-VPC. The second response
-- if for an account that supports both EC2-Classic and EC2-VPC.
-- https://ec2.amazonaws.com/?Action=DescribeAccountAttributes
-- &amp;AttributeName.1=supported-platforms &amp;AUTHPARAMS
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;supported-platforms&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;VPC&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;supported-platforms&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;EC2&lt;/attributeValue&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;attributeValue&gt;VPC&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt; Example 2 This example describes
-- the ID of your default VPC. The first response is for an account that
-- supports only EC2-VPC. The second response if for an account that supports
-- both EC2-Classic and EC2-VPC.
-- https://ec2.amazonaws.com/?Action=DescribeAccountAttributes
-- &amp;AttributeName.1=default-vpc &amp;AUTHPARAMS
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;default-vpc&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;vpc-xxxxxxxx&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;
-- &lt;DescribeAccountAttributesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;accountAttributeSet&gt; &lt;item&gt;
-- &lt;attributeName&gt;default-vpc&lt;/attributeName&gt;
-- &lt;attributeValueSet&gt; &lt;item&gt;
-- &lt;attributeValue&gt;none&lt;/attributeValue&gt; &lt;/item&gt;
-- &lt;/attributeValueSet&gt; &lt;/item&gt; &lt;/accountAttributeSet&gt;
-- &lt;/DescribeAccountAttributesResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DescribeAccountAttributes where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeAccountAttributes = DescribeAccountAttributes
    { _daarAttributeNames :: [AccountAttributeName]
      -- ^ One or more account attribute names.
    , _daarDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DescribeAccountAttributes where
    toQuery = genericToQuery def

instance AWSRequest DescribeAccountAttributes where
    type Sv DescribeAccountAttributes = EC2
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResponse

    request = post "DescribeAccountAttributes"
    response _ = xmlResponse

data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daasAccountAttributes :: [AccountAttribute]
      -- ^ Information about one or more account attributes.
    } deriving (Generic)

instance FromXML DescribeAccountAttributesResponse where
    fromXMLOptions = xmlOptions
