{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.DeleteMessageBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes multiple messages. This is a batch version of DeleteMessage. The
-- result of the delete action on each message is reported individually in the
-- response. Because the batch request can result in a combination of
-- successful and unsuccessful actions, you should check for batch errors even
-- when the call returns an HTTP status code of 200. Some API actions take
-- lists of parameters. These lists are specified using the param.n notation.
-- Values of n are integers starting from 1. For example, a parameter list
-- with two elements looks like this: &amp;Attribute.1=this
-- &amp;Attribute.2=that The following example DeleteMessageBatch request
-- deletes two messages. You must URL encode the entire URL; however, we've
-- URL encoded only the message body to make the example easier for you to
-- read. http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/
-- &Action=DeleteMessageBatch &Version=2011-10-01
-- &DeleteMessageBatchRequestEntry.1.Id=msg1
-- &DeleteMessageBatchRequestEntry.1.ReceiptHandle=gfk0T0R0waama4fVFffkjPQrrvzMrOg0fTFk2LxT33EuB8wR0ZCFgKWyXGWFoqqpCIiprQUEhir%2F5LeGPpYTLzjqLQxyQYaQALeSNHb0us3uE84uujxpBhsDkZUQkjFFkNqBXn48xlMcVhTcI3YLH%2Bd%2BIqetIOHgBCZAPx6r%2B09dWaBXei6nbK5Ygih21DCDdAwFV68Jo8DXhb3ErEfoDqx7vyvC5nCpdwqv%2BJhU%2FTNGjNN8t51v5c%2FAXvQsAzyZVNapxUrHIt4NxRhKJ72uICcxruyE8eRXlxIVNgeNP8ZEDcw7zZU1Zw%3D%3D
-- &DeleteMessageBatchRequestEntry.2.Id=msg2
-- &DeleteMessageBatchRequestEntry.2.ReceiptHandle=gfk0T0R0waama4fVFffkjKzmhMCymjQvfTFk2LxT33G4ms5subrE0deLKWSscPU1oD3J9zgeS4PQQ3U30qOumIE6AdAv3w%2F%2Fa1IXW6AqaWhGsEPaLm3Vf6IiWqdM8u5imB%2BNTwj3tQRzOWdTOePjOjPcTpRxBtXix%2BEvwJOZUma9wabv%2BSw6ZHjwmNcVDx8dZXJhVp16Bksiox%2FGrUvrVTCJRTWTLc59oHLLF8sEkKzRmGNzTDGTiV%2BYjHfQj60FD3rVaXmzTsoNxRhKJ72uIHVMGVQiAGgB%2BqAbSqfKHDQtVOmJJgkHug%3D%3D
-- &SignatureMethod=HmacSHA256 &Expires=2011-10-18T22%3A52%3A43PST
-- &AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &SignatureVersion=2
-- &Signature=Dqlp3Sd6ljTUA9Uf6SGtEExwUQEXAMPLE msg1 msg2
-- d6f86b7a-74d1-4439-b43f-196a1e29cd85.
module Network.AWS.SQS.DeleteMessageBatch
    (
    -- * Request
      DeleteMessageBatch
    -- ** Request constructor
    , deleteMessageBatch
    -- ** Request lenses
    , dmbQueueUrl
    , dmbEntries

    -- * Response
    , DeleteMessageBatchResponse
    -- ** Response constructor
    , deleteMessageBatchResponse
    -- ** Response lenses
    , dmbrSuccessful
    , dmbrFailed
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import Network.AWS.Prelude

data DeleteMessageBatch = DeleteMessageBatch
    { _dmbQueueUrl :: Text
    , _dmbEntries :: [DeleteMessageBatchRequestEntry]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMessageBatch' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @QueueUrl ::@ @Text@
--
-- * @Entries ::@ @[DeleteMessageBatchRequestEntry]@
--
deleteMessageBatch :: Text -- ^ 'dmbQueueUrl'
                   -> [DeleteMessageBatchRequestEntry] -- ^ 'dmbEntries'
                   -> DeleteMessageBatch
deleteMessageBatch p1 p2 = DeleteMessageBatch
    { _dmbQueueUrl = p1
    , _dmbEntries = p2
    }

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueUrl :: Lens' DeleteMessageBatch Text
dmbQueueUrl = lens _dmbQueueUrl (\s a -> s { _dmbQueueUrl = a })

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\s a -> s { _dmbEntries = a })

instance ToQuery DeleteMessageBatch where
    toQuery = genericQuery def

-- | For each message in the batch, the response contains a
-- DeleteMessageBatchResultEntry tag if the message is deleted or a
-- BatchResultErrorEntry tag if the message cannot be deleted.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse
    { _dmbrSuccessful :: [DeleteMessageBatchResultEntry]
    , _dmbrFailed :: [BatchResultErrorEntry]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMessageBatchResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Successful ::@ @[DeleteMessageBatchResultEntry]@
--
-- * @Failed ::@ @[BatchResultErrorEntry]@
--
deleteMessageBatchResponse :: [DeleteMessageBatchResultEntry] -- ^ 'dmbrSuccessful'
                           -> [BatchResultErrorEntry] -- ^ 'dmbrFailed'
                           -> DeleteMessageBatchResponse
deleteMessageBatchResponse p1 p2 = DeleteMessageBatchResponse
    { _dmbrSuccessful = p1
    , _dmbrFailed = p2
    }

-- | A list of DeleteMessageBatchResultEntry items.
dmbrSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrSuccessful = lens _dmbrSuccessful (\s a -> s { _dmbrSuccessful = a })

-- | A list of BatchResultErrorEntry items.
dmbrFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrFailed = lens _dmbrFailed (\s a -> s { _dmbrFailed = a })

instance FromXML DeleteMessageBatchResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteMessageBatch where
    type Sv DeleteMessageBatch = SQS
    type Rs DeleteMessageBatch = DeleteMessageBatchResponse

    request = post "DeleteMessageBatch"
    response _ = xmlResponse
