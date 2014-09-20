{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.ListTopics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the requester's topics. Each call returns a limited list
-- of topics, up to 100. If there are more topics, a NextToken is also
-- returned. Use the NextToken parameter in a new ListTopics call to get
-- further results. http://sns.us-east-1.amazonaws.com/ ?Action=ListTopics
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=tPg1qKNTNVPydnL3Yx5Fqm2O9GxCr9vh3EF5r9%2F5%2BJs%3D
-- &lt;ListTopicsResponse xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListTopicsResult&gt; &lt;Topics&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;/member&gt; &lt;/Topics&gt; &lt;/ListTopicsResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;3f1478c7-33a9-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListTopicsResponse&gt;.
module Network.AWS.SNS.ListTopics
    (
    -- * Request
      ListTopics
    -- ** Request constructor
    , listTopics
    -- ** Request lenses
    , ltNextToken

    -- * Response
    , ListTopicsResponse
    -- ** Response constructor
    , listTopicsResponse
    -- ** Response lenses
    , ltrTopics
    , ltrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

newtype ListTopics = ListTopics
    { _ltNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTopics' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NextToken ::@ @Maybe Text@
--
listTopics :: ListTopics
listTopics = ListTopics
    { _ltNextToken = Nothing
    }

-- | Token returned by the previous ListTopics request.
ltNextToken :: Lens' ListTopics (Maybe Text)
ltNextToken = lens _ltNextToken (\s a -> s { _ltNextToken = a })

instance ToQuery ListTopics where
    toQuery = genericQuery def

-- | Response for ListTopics action.
data ListTopicsResponse = ListTopicsResponse
    { _ltrTopics :: [Topic]
    , _ltrNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTopicsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Topics ::@ @[Topic]@
--
-- * @NextToken ::@ @Maybe Text@
--
listTopicsResponse :: ListTopicsResponse
listTopicsResponse = ListTopicsResponse
    { _ltrTopics = mempty
    , _ltrNextToken = Nothing
    }

-- | A list of topic ARNs.
ltrTopics :: Lens' ListTopicsResponse [Topic]
ltrTopics = lens _ltrTopics (\s a -> s { _ltrTopics = a })

-- | Token to pass along to the next ListTopics request. This element is
-- returned if there are additional topics to retrieve.
ltrNextToken :: Lens' ListTopicsResponse (Maybe Text)
ltrNextToken = lens _ltrNextToken (\s a -> s { _ltrNextToken = a })

instance FromXML ListTopicsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTopics where
    type Sv ListTopics = SNS
    type Rs ListTopics = ListTopicsResponse

    request = post "ListTopics"
    response _ = xmlResponse

instance AWSPager ListTopics where
    next rq rs = (\x -> rq & ltNextToken ?~ x)
        <$> (rs ^. ltrNextToken)
