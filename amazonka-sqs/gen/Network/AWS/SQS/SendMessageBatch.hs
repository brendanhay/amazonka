{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- even when the call returns an HTTP status code of 200.
-- &amp;Attribute.1=this &amp;Attribute.2=that.
module Network.AWS.SQS.SendMessageBatch
    (
    -- * Request
      SendMessageBatch
    -- ** Request constructor
    , sendMessageBatch
    -- ** Request lenses
    , smbEntries
    , smbQueueUrl

    -- * Response
    , SendMessageBatchResult
    -- ** Response constructor
    , sendMessageBatchResult
    -- ** Response lenses
    , smbrFailed
    , smbrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

data SendMessageBatch = SendMessageBatch
    { _smbEntries  :: [SendMessageBatchRequestEntry]
    , _smbQueueUrl :: Text
    } (Eq, Show, Generic)

-- | 'SendMessageBatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbEntries' @::@ ['SendMessageBatchRequestEntry']
--
-- * 'smbQueueUrl' @::@ 'Text'
--
sendMessageBatch :: Text -- ^ 'smbQueueUrl'
                 -> SendMessageBatch
sendMessageBatch p1 = SendMessageBatch
    { _smbQueueUrl = p1
    , _smbEntries  = mempty
    }

-- | A list of SendMessageBatchRequestEntry items.
smbEntries :: Lens' SendMessageBatch [SendMessageBatchRequestEntry]
smbEntries = lens _smbEntries (\s a -> s { _smbEntries = a })

-- | The URL of the Amazon SQS queue to take action on.
smbQueueUrl :: Lens' SendMessageBatch Text
smbQueueUrl = lens _smbQueueUrl (\s a -> s { _smbQueueUrl = a })
instance ToQuery SendMessageBatch

instance ToPath SendMessageBatch where
    toPath = const "/"

data SendMessageBatchResult = SendMessageBatchResult
    { _smbrFailed     :: [BatchResultErrorEntry]
    , _smbrSuccessful :: [SendMessageBatchResultEntry]
    } (Eq, Show, Generic)

-- | 'SendMessageBatchResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'smbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'smbrSuccessful' @::@ ['SendMessageBatchResultEntry']
--
sendMessageBatchResult :: SendMessageBatchResult
sendMessageBatchResult = SendMessageBatchResult
    { _smbrSuccessful = mempty
    , _smbrFailed     = mempty
    }

-- | A list of BatchResultErrorEntry items with the error detail about each
-- message that could not be enqueued.
smbrFailed :: Lens' SendMessageBatchResult [BatchResultErrorEntry]
smbrFailed = lens _smbrFailed (\s a -> s { _smbrFailed = a })

-- | A list of SendMessageBatchResultEntry items.
smbrSuccessful :: Lens' SendMessageBatchResult [SendMessageBatchResultEntry]
smbrSuccessful = lens _smbrSuccessful (\s a -> s { _smbrSuccessful = a })

instance FromXML SendMessageBatchResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SendMessageBatchResult"

instance AWSRequest SendMessageBatch where
    type Sv SendMessageBatch = SQS
    type Rs SendMessageBatch = SendMessageBatchResult

    request  = post "SendMessageBatch"
    response = xmlResponse $ \h x -> SendMessageBatchResult
        <$> x %| "Failed"
        <*> x %| "Successful"
