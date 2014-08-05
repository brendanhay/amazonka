{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.V2010_03_31.DeleteTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a topic and all its subscriptions. Deleting a topic might prevent
-- some messages previously sent to the topic from being delivered to
-- subscribers. This action is idempotent, so deleting a topic that does not
-- exist does not result in an error. http://sns.us-east-1.amazonaws.com/
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=DeleteTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D
-- &lt;DeleteTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;f3aa9ac9-3c3d-11df-8235-9dab105e9c32&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/DeleteTopicResponse&gt;.
module Network.AWS.SNS.V2010_03_31.DeleteTopic where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data DeleteTopic = DeleteTopic
    { _dtiTopicArn :: Text
      -- ^ The ARN of the topic you want to delete.
      -- http://sns.us-east-1.amazonaws.com/
      -- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
      -- &amp;Action=DeleteTopic &amp;SignatureVersion=2
      -- &amp;SignatureMethod=HmacSHA256
      -- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
      -- &amp;AWSAccessKeyId=(AWS Access Key ID)
      -- &amp;Signature=DjHBa%2BbYCKQAzctOPnLP7MbHnrHT3%2FK3kFEZjwcf9%2FU%3D
      -- &lt;DeleteTopicResponse
      -- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
      -- &lt;ResponseMetadata&gt;
      -- &lt;RequestId&gt;fba800b9-3765-11df-8cf3-c58c53254dfb&lt;/RequestId&gt;
      -- &lt;/ResponseMetadata&gt; &lt;/DeleteTopicResponse&gt;.
    } deriving (Show, Generic)

makeLenses ''DeleteTopic

instance ToQuery DeleteTopic where
    toQuery = genericToQuery def

data DeleteTopicResponse = DeleteTopicResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteTopicResponse

instance AWSRequest DeleteTopic where
    type Sv DeleteTopic = SNS
    type Rs DeleteTopic = DeleteTopicResponse

    request = post "DeleteTopic"
    response _ _ = return (Right DeleteTopicResponse)
