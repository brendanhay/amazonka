{-# LANGUAGE DataKinds                   #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes up to ten messages from the specified queue. This is a batch version
-- of 'DeleteMessage'. The result of the delete action on each message is reported
-- individually in the response.
--
-- Because the batch request can result in a combination of successful and
-- unsuccessful actions, you should check for batch errors even when the call
-- returns an HTTP status code of 200.
--
-- Some API actions take lists of parameters. These lists are specified using
-- the 'param.n' notation. Values of 'n' are integers starting from 1. For example,
-- a parameter list with two elements looks like this:  '&Attribute.1=this'
--
-- '&Attribute.2=that'
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
    { _dmbEntries  :: List "member" DeleteMessageBatchRequestEntry
    , _dmbQueueUrl :: Text
    } deriving (Eq, Read, Show)

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
dmbEntries = lens _dmbEntries (\s a -> s { _dmbEntries = a }) . _List

-- | The URL of the Amazon SQS queue to take action on.
dmbQueueUrl :: Lens' DeleteMessageBatch Text
dmbQueueUrl = lens _dmbQueueUrl (\s a -> s { _dmbQueueUrl = a })

data DeleteMessageBatchResponse = DeleteMessageBatchResponse
    { _dmbrFailed     :: List "member" BatchResultErrorEntry
    , _dmbrSuccessful :: List "member" DeleteMessageBatchResultEntry
    } deriving (Eq, Read, Show)

-- | 'DeleteMessageBatchResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'dmbrSuccessful' @::@ ['DeleteMessageBatchResultEntry']
--
deleteMessageBatchResponse :: DeleteMessageBatchResponse
deleteMessageBatchResponse = DeleteMessageBatchResponse
    { _dmbrSuccessful = mempty
    , _dmbrFailed     = mempty
    }

-- | A list of 'BatchResultErrorEntry' items.
dmbrFailed :: Lens' DeleteMessageBatchResponse [BatchResultErrorEntry]
dmbrFailed = lens _dmbrFailed (\s a -> s { _dmbrFailed = a }) . _List

-- | A list of 'DeleteMessageBatchResultEntry' items.
dmbrSuccessful :: Lens' DeleteMessageBatchResponse [DeleteMessageBatchResultEntry]
dmbrSuccessful = lens _dmbrSuccessful (\s a -> s { _dmbrSuccessful = a }) . _List

instance ToPath DeleteMessageBatch where
    toPath = const "/"

instance ToQuery DeleteMessageBatch where
    toQuery DeleteMessageBatch{..} = mconcat
        [ toQuery   _dmbEntries
        , "QueueUrl" =? _dmbQueueUrl
        ]

instance ToHeaders DeleteMessageBatch

instance AWSRequest DeleteMessageBatch where
    type Sv DeleteMessageBatch = SQS
    type Rs DeleteMessageBatch = DeleteMessageBatchResponse

    request  = post "DeleteMessageBatch"
    response = xmlResponse

instance FromXML DeleteMessageBatchResponse where
    parseXML = withElement "DeleteMessageBatchResult" $ \x -> DeleteMessageBatchResponse
        <$> parseXML x
        <*> parseXML x
