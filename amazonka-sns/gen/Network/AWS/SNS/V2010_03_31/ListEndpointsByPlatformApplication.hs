{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the endpoints and endpoint attributes for devices in a supported push
-- notification service, such as GCM and APNS. The results for
-- ListEndpointsByPlatformApplication are paginated and return a limited list
-- of endpoints, up to 100. If additional records are available after the
-- first page results, then a NextToken string will be returned. To receive
-- the next page, you call ListEndpointsByPlatformApplication again using the
-- NextToken string received from the previous call. When there are no more
-- records to return, NextToken will be null. For more information, see Using
-- Amazon SNS Mobile Push Notifications. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=ListEndpointsByPlatformApplication
-- &amp;SignatureMethod=HmacSHA256 &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=e6H4sJSCRBBlh%2BaigB%2FtYgp4%2Bjl7dikAQ6WKf%2BMTwNM%3D
-- &amp;Timestamp=2013-07-01T23%3A00%3A52.515Z HTTP/1.1 200 OK ...
-- &lt;ListEndpointsByPlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListEndpointsByPlatformApplicationResult&gt; &lt;Endpoints&gt;
-- &lt;member&gt;
-- &lt;EndpointArn&gt;arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3&lt;/EndpointArn&gt;
-- &lt;Attributes&gt; &lt;entry&gt; &lt;key&gt;Enabled&lt;/key&gt;
-- &lt;value&gt;true&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;CustomUserData&lt;/key&gt;
-- &lt;value&gt;UserId=27576823&lt;/value&gt; &lt;/entry&gt; &lt;entry&gt;
-- &lt;key&gt;Token&lt;/key&gt;
-- &lt;value&gt;APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/member&gt; &lt;/Endpoints&gt;
-- &lt;/ListEndpointsByPlatformApplicationResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;9a48768c-dac8-5a60-aec0-3cc27ea08d96&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt;
-- &lt;/ListEndpointsByPlatformApplicationResponse&gt;.
module Network.AWS.SNS.V2010_03_31.ListEndpointsByPlatformApplication
    (
    -- * Request
      ListEndpointsByPlatformApplication
    -- ** Request constructor
    , mkListEndpointsByPlatformApplicationInput
    -- ** Request lenses
    , lebpaiPlatformApplicationArn
    , lebpaiNextToken

    -- * Response
    , ListEndpointsByPlatformApplicationResponse
    -- ** Response lenses
    , lebparEndpoints
    , lebparNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListEndpointsByPlatformApplication' request.
mkListEndpointsByPlatformApplicationInput :: Text -- ^ 'lebpaiPlatformApplicationArn'
                                          -> ListEndpointsByPlatformApplication
mkListEndpointsByPlatformApplicationInput p1 = ListEndpointsByPlatformApplication
    { _lebpaiPlatformApplicationArn = p1
    , _lebpaiNextToken = Nothing
    }
{-# INLINE mkListEndpointsByPlatformApplicationInput #-}

data ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplication
    { _lebpaiPlatformApplicationArn :: Text
      -- ^ PlatformApplicationArn for
      -- ListEndpointsByPlatformApplicationInput action.
    , _lebpaiNextToken :: Maybe Text
      -- ^ NextToken string is used when calling
      -- ListEndpointsByPlatformApplication action to retrieve additional
      -- records that are available after the first page results.
    } deriving (Show, Generic)

-- | PlatformApplicationArn for ListEndpointsByPlatformApplicationInput action.
lebpaiPlatformApplicationArn :: Lens' ListEndpointsByPlatformApplication (Text)
lebpaiPlatformApplicationArn = lens _lebpaiPlatformApplicationArn (\s a -> s { _lebpaiPlatformApplicationArn = a })
{-# INLINE lebpaiPlatformApplicationArn #-}

-- | NextToken string is used when calling ListEndpointsByPlatformApplication
-- action to retrieve additional records that are available after the first
-- page results.
lebpaiNextToken :: Lens' ListEndpointsByPlatformApplication (Maybe Text)
lebpaiNextToken = lens _lebpaiNextToken (\s a -> s { _lebpaiNextToken = a })
{-# INLINE lebpaiNextToken #-}

instance ToQuery ListEndpointsByPlatformApplication where
    toQuery = genericQuery def

data ListEndpointsByPlatformApplicationResponse = ListEndpointsByPlatformApplicationResponse
    { _lebparEndpoints :: [Endpoint]
      -- ^ Endpoints returned for ListEndpointsByPlatformApplication action.
    , _lebparNextToken :: Maybe Text
      -- ^ NextToken string is returned when calling
      -- ListEndpointsByPlatformApplication action if additional records
      -- are available after the first page results.
    } deriving (Show, Generic)

-- | Endpoints returned for ListEndpointsByPlatformApplication action.
lebparEndpoints :: Lens' ListEndpointsByPlatformApplicationResponse ([Endpoint])
lebparEndpoints = lens _lebparEndpoints (\s a -> s { _lebparEndpoints = a })
{-# INLINE lebparEndpoints #-}

-- | NextToken string is returned when calling
-- ListEndpointsByPlatformApplication action if additional records are
-- available after the first page results.
lebparNextToken :: Lens' ListEndpointsByPlatformApplicationResponse (Maybe Text)
lebparNextToken = lens _lebparNextToken (\s a -> s { _lebparNextToken = a })
{-# INLINE lebparNextToken #-}

instance FromXML ListEndpointsByPlatformApplicationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListEndpointsByPlatformApplication where
    type Sv ListEndpointsByPlatformApplication = SNS
    type Rs ListEndpointsByPlatformApplication = ListEndpointsByPlatformApplicationResponse

    request = post "ListEndpointsByPlatformApplication"
    response _ = xmlResponse

instance AWSPager ListEndpointsByPlatformApplication where
    next rq rs = (\x -> rq { _lebpaiNextToken = Just x })
        <$> (_lebparNextToken rs)
