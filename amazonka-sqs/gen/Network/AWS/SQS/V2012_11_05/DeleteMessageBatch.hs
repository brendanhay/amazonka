{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.V2012_11_05.DeleteMessageBatch
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
module Network.AWS.SQS.V2012_11_05.DeleteMessageBatch
    (
    -- * Request
      DeleteMessageBatch
    -- ** Request constructor
    , mkDeleteMessageBatch
    -- ** Request lenses
    , dmbQueueUrl
    , dmbEntries

    -- * Response
    , DeleteMessageBatchResponse
    -- ** Response lenses
    , dmbrsSuccessful
    , dmbrsFailed
    ) where

import Network.AWS.Request.Query
import Network.AWS.SQS.V2012_11_05.Types
import Network.AWS.Prelude

-- | 
data DeleteMessageBatch = DeleteMessageBatch
    { _dmbQueueUrl :: Text
    , _dmbEntries :: [DeleteMessageBatchRequestEntry]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMessageBatch' request.
mkDeleteMessageBatch :: Text -- ^ 'dmbQueueUrl'
                     -> [DeleteMessageBatchRequestEntry] -- ^ 'dmbEntries'
                     -> DeleteMessageBatch
mkDeleteMessageBatch p1 p2 = DeleteMessageBatch
    { _dmbQueueUrl = p1
    , _dmbEntries = p2
    }
{-# INLINE mkDeleteMessageBatch #-}

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueUrl :: Lens' DeleteMessageBatch Text
dmbQueueUrl = lens _dmbQueueUrl (\s a -> s { _dmbQueueUrl = a })
{-# INLINE dmbQueueUrl #-}

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\s a -> s { _dmbEntries = a })
{-# INLINE dmbEntries #-}

instance ToQuery DeleteMessageBatch where
    toQuery = genericQuery def

-- | For each message in the batch, the response contains a
-- DeleteMessageBatchResultEntry tag if the message is deleted or a
-- BatchResultErrorEntry tag if the message cannot be deleted.
data DeleteMessageBatchResponse = DeleteMessageBatchResponse
    { _dmbrsSuccessful :: [DeleteMessageBatchResultEntry]
    , _dmbrsFailed :: [BatchResultErrorEntry]
    } deriving (Show, Generic)

-- | A list of DeleteMessageBatchResultEntry items.
dmbrsSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrsSuccessful = lens _dmbrsSuccessful (\s a -> s { _dmbrsSuccessful = a })
{-# INLINE dmbrsSuccessful #-}

-- | A list of BatchResultErrorEntry items.
dmbrsFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrsFailed = lens _dmbrsFailed (\s a -> s { _dmbrsFailed = a })
{-# INLINE dmbrsFailed #-}

instance FromXML DeleteMessageBatchResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteMessageBatch where
    type Sv DeleteMessageBatch = SQS
    type Rs DeleteMessageBatch = DeleteMessageBatchResponse

    request = post "DeleteMessageBatch"
    response _ = xmlResponse
