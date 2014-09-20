{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.SendMessageBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Delivers up to ten messages to the specified queue. This is a batch version
-- of SendMessage. The result of the send action on each message is reported
-- individually in the response. The maximum allowed individual message size
-- is 256 KB (262,144 bytes). The maximum total payload size (i.e., the sum of
-- all a batch's individual message lengths) is also 256 KB (262,144 bytes).
-- If the DelaySeconds parameter is not specified for an entry, the default
-- for the queue is used. The following list shows the characters (in Unicode)
-- that are allowed in your message, according to the W3C XML specification.
-- For more information, go to http://www.faqs.org/rfcs/rfc1321.html. If you
-- send any characters that are not included in the list, your request will be
-- rejected. #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] |
-- [#x10000 to #x10FFFF] Because the batch request can result in a combination
-- of successful and unsuccessful actions, you should check for batch errors
-- even when the call returns an HTTP status code of 200. Some API actions
-- take lists of parameters. These lists are specified using the param.n
-- notation. Values of n are integers starting from 1. For example, a
-- parameter list with two elements looks like this: &amp;Attribute.1=this
-- &amp;Attribute.2=that The following example SendMessageBatch request sends
-- two messages to the queue. You must URL encode the entire URL; however,
-- we've URL encoded only the message body to make the example easier for you
-- to read. The following example response includes the MD5 digest for the
-- messages. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- ?Action=SendMessageBatch &SendMessageBatchRequestEntry.1.Id=test_msg_001
-- &SendMessageBatchRequestEntry.1.MessageBody=test%20message%20body%201
-- &SendMessageBatchRequestEntry.2.Id=test_msg_002
-- &SendMessageBatchRequestEntry.2.MessageBody=test%20message%20body%202
-- &SendMessageBatchRequestEntry.2.DelaySeconds=60
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Name=test_attribute_name_1
-- 
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.StringValue=test_attribute_value_1
-- &SendMessageBatchRequestEntry.2.MessageAttribute.1.Value.DataType=String
-- &Version=2012-11-05 &SignatureMethod=HmacSHA256
-- &Expires=2014-05-05T22%3A52%3A43PST &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &SignatureVersion=2 &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE
-- test_msg_001 0a5231c7-8bff-4955-be2e-8dc7c50a25fa
-- 0e024d309850c78cba5eabbeff7cae71 test_msg_002
-- 15ee1ed3-87e7-40c1-bdaa-2e49968ea7e9 7fb8146a82f95e0af155278f406862c2
-- 295c5fa15a51aae6884d1d7c1d99ca50 ca1ad5d0-8271-408b-8d0f-1351bf547e74.
module Network.AWS.SQS.SendMessageBatch
    (
    -- * Request
      SendMessageBatch
    -- ** Request constructor
    , sendMessageBatch
    -- ** Request lenses
    , smbQueueUrl
    , smbEntries

    -- * Response
    , SendMessageBatchResponse
    -- ** Response constructor
    , sendMessageBatchResponse
    -- ** Response lenses
    , smbrSuccessful
    , smbrFailed
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data SendMessageBatch = SendMessageBatch
    { _smbQueueUrl :: Text
    , _smbEntries :: [SendMessageBatchRequestEntry]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendMessageBatch' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @Entries ::@ @[SendMessageBatchRequestEntry]@
--
sendMessageBatch :: Text -- ^ 'smbQueueUrl'
                 -> [SendMessageBatchRequestEntry] -- ^ 'smbEntries'
                 -> SendMessageBatch
sendMessageBatch p1 p2 = SendMessageBatch
    { _smbQueueUrl = p1
    , _smbEntries = p2
    }

-- | The URL of the Amazon SQS queue to take action on.
smbQueueUrl :: Lens' SendMessageBatch Text
smbQueueUrl = lens _smbQueueUrl (\s a -> s { _smbQueueUrl = a })

-- | A list of SendMessageBatchRequestEntry items.
smbEntries :: Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = lens _smbEntries (\s a -> s { _smbEntries = a })

instance ToQuery SendMessageBatch where
    toQuery = genericQuery def

-- | For each message in the batch, the response contains a
-- SendMessageBatchResultEntry tag if the message succeeds or a
-- BatchResultErrorEntry tag if the message fails.
data SendMessageBatchResponse = SendMessageBatchResponse
    { _smbrSuccessful :: [SendMessageBatchResultEntry]
    , _smbrFailed :: [BatchResultErrorEntry]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendMessageBatchResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Successful ::@ @[SendMessageBatchResultEntry]@
--
-- * @Failed ::@ @[BatchResultErrorEntry]@
--
sendMessageBatchResponse :: [SendMessageBatchResultEntry] -- ^ 'smbrSuccessful'
                         -> [BatchResultErrorEntry] -- ^ 'smbrFailed'
                         -> SendMessageBatchResponse
sendMessageBatchResponse p1 p2 = SendMessageBatchResponse
    { _smbrSuccessful = p1
    , _smbrFailed = p2
    }

-- | A list of SendMessageBatchResultEntry items.
smbrSuccessful :: Lens' SendMessageBatchResponse [SendMessageBatchResultEntry]
smbrSuccessful = lens _smbrSuccessful (\s a -> s { _smbrSuccessful = a })

-- | A list of BatchResultErrorEntry items with the error detail about each
-- message that could not be enqueued.
smbrFailed :: Lens' SendMessageBatchResponse [BatchResultErrorEntry]
smbrFailed = lens _smbrFailed (\s a -> s { _smbrFailed = a })

instance FromXML SendMessageBatchResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest SendMessageBatch where
    type Sv SendMessageBatch = SQS
    type Rs SendMessageBatch = SendMessageBatchResponse

    request = post "SendMessageBatch"
    response _ = xmlResponse
