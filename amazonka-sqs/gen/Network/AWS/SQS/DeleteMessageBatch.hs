{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- when the call returns an HTTP status code of 200. &amp;Attribute.1=this
-- &amp;Attribute.2=that.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessageBatch.html>
module Network.AWS.SQS.DeleteMessageBatch
    (
    -- * Request
      DeleteMessageBatch
    -- ** Request constructor
    , deleteMessageBatch
    -- ** Request lenses
    , dmbEntries
    , dmbQueueUrl

    -- * Response
    , DeleteMessageBatchResponse
    -- ** Response constructor
    , deleteMessageBatchResponse
    -- ** Response lenses
    , dmbrFailed
    , dmbrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data DeleteMessageBatch = DeleteMessageBatch
    { _dmbEntries  :: Flatten [DeleteMessageBatchRequestEntry]
    , _dmbQueueUrl :: Text
    } deriving (Eq, Show, Generic)

-- | 'DeleteMessageBatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbEntries' @::@ ['DeleteMessageBatchRequestEntry']
--
-- * 'dmbQueueUrl' @::@ 'Text'
--
deleteMessageBatch :: Text -- ^ 'dmbQueueUrl'
                   -> [DeleteMessageBatchRequestEntry] -- ^ 'dmbEntries'
                   -> DeleteMessageBatch
deleteMessageBatch p1 p2 = DeleteMessageBatch
    { _dmbQueueUrl = p1
    , _dmbEntries  = withIso _Flatten (const id) p2
    }

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\s a -> s { _dmbEntries = a })
    . _Flatten

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueUrl :: Lens' DeleteMessageBatch Text
dmbQueueUrl = lens _dmbQueueUrl (\s a -> s { _dmbQueueUrl = a })

data DeleteMessageBatchResponse = DeleteMessageBatchResponse
    { _dmbrFailed     :: Flatten [BatchResultErrorEntry]
    , _dmbrSuccessful :: Flatten [DeleteMessageBatchResultEntry]
    } deriving (Eq, Show, Generic)

-- | 'DeleteMessageBatchResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'dmbrSuccessful' @::@ ['DeleteMessageBatchResultEntry']
--
deleteMessageBatchResponse :: [DeleteMessageBatchResultEntry] -- ^ 'dmbrSuccessful'
                           -> [BatchResultErrorEntry] -- ^ 'dmbrFailed'
                           -> DeleteMessageBatchResponse
deleteMessageBatchResponse p1 p2 = DeleteMessageBatchResponse
    { _dmbrSuccessful = withIso _Flatten (const id) p1
    , _dmbrFailed     = withIso _Flatten (const id) p2
    }

-- | A list of BatchResultErrorEntry items.
dmbrFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrFailed = lens _dmbrFailed (\s a -> s { _dmbrFailed = a })
    . _Flatten

-- | A list of DeleteMessageBatchResultEntry items.
dmbrSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrSuccessful = lens _dmbrSuccessful (\s a -> s { _dmbrSuccessful = a })
    . _Flatten

instance ToPath DeleteMessageBatch where
    toPath = const "/"

instance ToQuery DeleteMessageBatch

instance ToHeaders DeleteMessageBatch

instance AWSRequest DeleteMessageBatch where
    type Sv DeleteMessageBatch = SQS
    type Rs DeleteMessageBatch = DeleteMessageBatchResponse

    request  = post "DeleteMessageBatch"
    response = xmlResponse

instance FromXML DeleteMessageBatchResponse where
    parseXML = withElement "DeleteMessageBatchResult" $ \x ->
        DeleteMessageBatchResponse
            <$> parseXML x
            <*> parseXML x
