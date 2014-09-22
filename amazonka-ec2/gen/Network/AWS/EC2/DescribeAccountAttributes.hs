{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAccountAttributes
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
module Network.AWS.EC2.DescribeAccountAttributes
    (
    -- * Request
      DescribeAccountAttributes
    -- ** Request constructor
    , describeAccountAttributes
    -- ** Request lenses
    , daaAttributeName

    -- * Response
    , DescribeAccountAttributesResponse
    -- ** Response constructor
    , describeAccountAttributesResponse
    -- ** Response lenses
    , daarItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DescribeAccountAttributes = DescribeAccountAttributes
    { _daaAttributeName :: [AccountAttributeName]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAccountAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AttributeName ::@ @[AccountAttributeName]@
--
describeAccountAttributes :: DescribeAccountAttributes
describeAccountAttributes = DescribeAccountAttributes
    { _daaAttributeName = mempty
    }

-- | One or more account attribute names.
daaAttributeName :: Lens' DescribeAccountAttributes [AccountAttributeName]
daaAttributeName =
    lens _daaAttributeName (\s a -> s { _daaAttributeName = a })

instance ToQuery DescribeAccountAttributes where
    toQuery = genericQuery def

newtype DescribeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarItem :: [AccountAttribute]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAccountAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[AccountAttribute]@
--
describeAccountAttributesResponse :: DescribeAccountAttributesResponse
describeAccountAttributesResponse = DescribeAccountAttributesResponse
    { _daarItem = mempty
    }

-- | Information about one or more account attributes.
daarItem :: Lens' DescribeAccountAttributesResponse [AccountAttribute]
daarItem = lens _daarItem (\s a -> s { _daarItem = a })

instance FromXML DescribeAccountAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAccountAttributes where
    type Sv DescribeAccountAttributes = EC2
    type Rs DescribeAccountAttributes = DescribeAccountAttributesResponse

    request = post "DescribeAccountAttributes"
    response _ = xmlResponse
