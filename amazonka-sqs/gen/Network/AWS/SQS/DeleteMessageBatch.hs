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
    , DeleteMessageBatchResult
    -- ** Response constructor
    , deleteMessageBatchResult
    -- ** Response lenses
    , dmbrFailed
    , dmbrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

data DeleteMessageBatch = DeleteMessageBatch
    { _dmbEntries  :: [DeleteMessageBatchRequestEntry]
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
                   -> DeleteMessageBatch
deleteMessageBatch p1 = DeleteMessageBatch
    { _dmbQueueUrl = p1
    , _dmbEntries  = mempty
    }

-- | A list of receipt handles for the messages to be deleted.
dmbEntries :: Lens' DeleteMessageBatch [DeleteMessageBatchRequestEntry]
dmbEntries = lens _dmbEntries (\s a -> s { _dmbEntries = a })

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueUrl :: Lens' DeleteMessageBatch Text
dmbQueueUrl = lens _dmbQueueUrl (\s a -> s { _dmbQueueUrl = a })

instance ToQuery DeleteMessageBatch

instance ToPath DeleteMessageBatch where
    toPath = const "/"

data DeleteMessageBatchResult = DeleteMessageBatchResult
    { _dmbrFailed     :: [BatchResultErrorEntry]
    , _dmbrSuccessful :: [DeleteMessageBatchResultEntry]
    } deriving (Eq, Show, Generic)

-- | 'DeleteMessageBatchResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'dmbrSuccessful' @::@ ['DeleteMessageBatchResultEntry']
--
deleteMessageBatchResult :: DeleteMessageBatchResult
deleteMessageBatchResult = DeleteMessageBatchResult
    { _dmbrSuccessful = mempty
    , _dmbrFailed     = mempty
    }

-- | A list of BatchResultErrorEntry items.
dmbrFailed :: Lens' DeleteMessageBatchResult [BatchResultErrorEntry]
dmbrFailed = lens _dmbrFailed (\s a -> s { _dmbrFailed = a })

-- | A list of DeleteMessageBatchResultEntry items.
dmbrSuccessful :: Lens' DeleteMessageBatchResult [DeleteMessageBatchResultEntry]
dmbrSuccessful = lens _dmbrSuccessful (\s a -> s { _dmbrSuccessful = a })

instance FromXML DeleteMessageBatchResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteMessageBatchResult"

instance AWSRequest DeleteMessageBatch where
    type Sv DeleteMessageBatch = SQS
    type Rs DeleteMessageBatch = DeleteMessageBatchResult

    request  = post "DeleteMessageBatch"
    response = xmlResponse $ \h x -> DeleteMessageBatchResult
        <$> x %| "Failed"
        <*> x %| "Successful"
