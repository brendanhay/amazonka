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

-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
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

-- | Changes the visibility timeout of multiple messages. This is a batch version
-- of 'ChangeMessageVisibility'. The result of the action on each message is
-- reported individually in the response. You can send up to 10 'ChangeMessageVisibility' requests with each 'ChangeMessageVisibilityBatch' action.
--
-- Because the batch request can result in a combination of successful and
-- unsuccessful actions, you should check for batch errors even when the call
-- returns an HTTP status code of 200. Some API actions take lists of
-- parameters. These lists are specified using the 'param.n' notation. Values of 'n'
-- are integers starting from 1. For example, a parameter list with two elements
-- looks like this:  '&Attribute.1=this'
--
-- '&Attribute.2=that'
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ChangeMessageVisibilityBatch.html>
module Network.AWS.SQS.ChangeMessageVisibilityBatch
    (
    -- * Request
      ChangeMessageVisibilityBatch
    -- ** Request constructor
    , changeMessageVisibilityBatch
    -- ** Request lenses
    , cmvbEntries
    , cmvbQueueUrl

    -- * Response
    , ChangeMessageVisibilityBatchResponse
    -- ** Response constructor
    , changeMessageVisibilityBatchResponse
    -- ** Response lenses
    , cmvbrFailed
    , cmvbrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch
    { _cmvbEntries  :: List "member" ChangeMessageVisibilityBatchRequestEntry
    , _cmvbQueueUrl :: Text
    } deriving (Eq, Show)

-- | 'ChangeMessageVisibilityBatch' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbEntries' @::@ ['ChangeMessageVisibilityBatchRequestEntry']
--
-- * 'cmvbQueueUrl' @::@ 'Text'
--
changeMessageVisibilityBatch :: Text -- ^ 'cmvbQueueUrl'
                             -> ChangeMessageVisibilityBatch
changeMessageVisibilityBatch p1 = ChangeMessageVisibilityBatch
    { _cmvbQueueUrl = p1
    , _cmvbEntries  = mempty
    }

-- | A list of receipt handles of the messages for which the visibility timeout
-- must be changed.
cmvbEntries :: Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = lens _cmvbEntries (\s a -> s { _cmvbEntries = a }) . _List

-- | The URL of the Amazon SQS queue to take action on.
cmvbQueueUrl :: Lens' ChangeMessageVisibilityBatch Text
cmvbQueueUrl = lens _cmvbQueueUrl (\s a -> s { _cmvbQueueUrl = a })

data ChangeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse
    { _cmvbrFailed     :: List "member" BatchResultErrorEntry
    , _cmvbrSuccessful :: List "member" ChangeMessageVisibilityBatchResultEntry
    } deriving (Eq, Show)

-- | 'ChangeMessageVisibilityBatchResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'cmvbrSuccessful' @::@ ['ChangeMessageVisibilityBatchResultEntry']
--
changeMessageVisibilityBatchResponse :: ChangeMessageVisibilityBatchResponse
changeMessageVisibilityBatchResponse = ChangeMessageVisibilityBatchResponse
    { _cmvbrSuccessful = mempty
    , _cmvbrFailed     = mempty
    }

-- | A list of 'BatchResultErrorEntry' items.
cmvbrFailed :: Lens' ChangeMessageVisibilityBatchResponse [BatchResultErrorEntry]
cmvbrFailed = lens _cmvbrFailed (\s a -> s { _cmvbrFailed = a }) . _List

-- | A list of 'ChangeMessageVisibilityBatchResultEntry' items.
cmvbrSuccessful :: Lens' ChangeMessageVisibilityBatchResponse [ChangeMessageVisibilityBatchResultEntry]
cmvbrSuccessful = lens _cmvbrSuccessful (\s a -> s { _cmvbrSuccessful = a }) . _List

instance ToPath ChangeMessageVisibilityBatch where
    toPath = const "/"

instance ToQuery ChangeMessageVisibilityBatch where
    toQuery ChangeMessageVisibilityBatch{..} = mconcat
        [ toQuery   _cmvbEntries
        , "QueueUrl" =? _cmvbQueueUrl
        ]

instance ToHeaders ChangeMessageVisibilityBatch

instance AWSRequest ChangeMessageVisibilityBatch where
    type Sv ChangeMessageVisibilityBatch = SQS
    type Rs ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatchResponse

    request  = post "ChangeMessageVisibilityBatch"
    response = xmlResponse

instance FromXML ChangeMessageVisibilityBatchResponse where
    parseXML = withElement "ChangeMessageVisibilityBatchResult" $ \x -> ChangeMessageVisibilityBatchResponse
        <$> parseXML x
        <*> parseXML x
