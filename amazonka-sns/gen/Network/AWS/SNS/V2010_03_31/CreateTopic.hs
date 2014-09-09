{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.CreateTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a topic to which notifications can be published. Users can create
-- at most 3000 topics. For more information, see http://aws.amazon.com/sns.
-- This action is idempotent, so if the requester already owns a topic with
-- the specified name, that topic's ARN is returned without creating a new
-- topic. http://sns.us-east-1.amazonaws.com/ ?Name=My-Topic
-- &amp;Action=CreateTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=gfzIF53exFVdpSNb8AiwN3Lv%2FNYXh6S%2Br3yySK70oX4%3D
-- &lt;CreateTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreateTopicResult&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;/CreateTopicResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8dec8b3-33a4-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreateTopicResponse&gt;.
module Network.AWS.SNS.V2010_03_31.CreateTopic
    (
    -- * Request
      CreateTopic
    -- ** Request constructor
    , mkCreateTopic
    -- ** Request lenses
    , ctName

    -- * Response
    , CreateTopicResponse
    -- ** Response constructor
    , mkCreateTopicResponse
    -- ** Response lenses
    , ctrTopicArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Input for CreateTopic action.
newtype CreateTopic = CreateTopic
    { _ctName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTopic' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
mkCreateTopic :: Text -- ^ 'ctName'
              -> CreateTopic
mkCreateTopic p1 = CreateTopic
    { _ctName = p1
    }

-- | The name of the topic you want to create. Constraints: Topic names must be
-- made up of only uppercase and lowercase ASCII letters, numbers,
-- underscores, and hyphens, and must be between 1 and 256 characters long.
ctName :: Lens' CreateTopic Text
ctName = lens _ctName (\s a -> s { _ctName = a })

instance ToQuery CreateTopic where
    toQuery = genericQuery def

-- | Response from CreateTopic action.
newtype CreateTopicResponse = CreateTopicResponse
    { _ctrTopicArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateTopicResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Maybe Text@
--
mkCreateTopicResponse :: CreateTopicResponse
mkCreateTopicResponse = CreateTopicResponse
    { _ctrTopicArn = Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the created topic.
ctrTopicArn :: Lens' CreateTopicResponse (Maybe Text)
ctrTopicArn = lens _ctrTopicArn (\s a -> s { _ctrTopicArn = a })

instance FromXML CreateTopicResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateTopic where
    type Sv CreateTopic = SNS
    type Rs CreateTopic = CreateTopicResponse

    request = post "CreateTopic"
    response _ = xmlResponse
