{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.SNS.V2010_03_31.CreateTopic where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data CreateTopic = CreateTopic
    { _ctiName :: Text
      -- ^ The name of the topic you want to create. Constraints: Topic
      -- names must be made up of only uppercase and lowercase ASCII
      -- letters, numbers, underscores, and hyphens, and must be between 1
      -- and 256 characters long.
    } deriving (Show, Generic)

makeLenses ''CreateTopic

instance ToQuery CreateTopic where
    toQuery = genericToQuery def

data CreateTopicResponse = CreateTopicResponse
    { _ctrTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) assigned to the created topic.
    } deriving (Show, Generic)

makeLenses ''CreateTopicResponse

instance AWSRequest CreateTopic where
    type Sv CreateTopic = SNS
    type Rs CreateTopic = CreateTopicResponse

    request = post "CreateTopic"
    response _ = cursorResponse $ \hs xml ->
        pure CreateTopicResponse
            <*> xml %|? "TopicARN"
