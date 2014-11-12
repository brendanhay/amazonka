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

-- Module      : Network.AWS.SQS.ChangeMessageVisibilityBatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the visibility timeout of multiple messages. This is a batch
-- version of ChangeMessageVisibility. The result of the action on each
-- message is reported individually in the response. You can send up to 10
-- ChangeMessageVisibility requests with each ChangeMessageVisibilityBatch
-- action. Because the batch request can result in a combination of successful
-- and unsuccessful actions, you should check for batch errors even when the
-- call returns an HTTP status code of 200. &amp;Attribute.1=this
-- &amp;Attribute.2=that.
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
    , ChangeMessageVisibilityBatchResult
    -- ** Response constructor
    , changeMessageVisibilityBatchResult
    -- ** Response lenses
    , cmvbrFailed
    , cmvbrSuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types

data ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatch
    { _cmvbEntries  :: [ChangeMessageVisibilityBatchRequestEntry]
    , _cmvbQueueUrl :: Text
    } deriving (Eq, Show, Generic)

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

-- | A list of receipt handles of the messages for which the visibility
-- timeout must be changed.
cmvbEntries :: Lens' ChangeMessageVisibilityBatch [ChangeMessageVisibilityBatchRequestEntry]
cmvbEntries = lens _cmvbEntries (\s a -> s { _cmvbEntries = a })

-- | The URL of the Amazon SQS queue to take action on.
cmvbQueueUrl :: Lens' ChangeMessageVisibilityBatch Text
cmvbQueueUrl = lens _cmvbQueueUrl (\s a -> s { _cmvbQueueUrl = a })

instance ToQuery ChangeMessageVisibilityBatch

instance ToPath ChangeMessageVisibilityBatch where
    toPath = const "/"

data ChangeMessageVisibilityBatchResult = ChangeMessageVisibilityBatchResult
    { _cmvbrFailed     :: [BatchResultErrorEntry]
    , _cmvbrSuccessful :: [ChangeMessageVisibilityBatchResultEntry]
    } deriving (Eq, Show, Generic)

-- | 'ChangeMessageVisibilityBatchResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvbrFailed' @::@ ['BatchResultErrorEntry']
--
-- * 'cmvbrSuccessful' @::@ ['ChangeMessageVisibilityBatchResultEntry']
--
changeMessageVisibilityBatchResult :: ChangeMessageVisibilityBatchResult
changeMessageVisibilityBatchResult = ChangeMessageVisibilityBatchResult
    { _cmvbrSuccessful = mempty
    , _cmvbrFailed     = mempty
    }

-- | A list of BatchResultErrorEntry items.
cmvbrFailed :: Lens' ChangeMessageVisibilityBatchResult [BatchResultErrorEntry]
cmvbrFailed = lens _cmvbrFailed (\s a -> s { _cmvbrFailed = a })

-- | A list of ChangeMessageVisibilityBatchResultEntry items.
cmvbrSuccessful :: Lens' ChangeMessageVisibilityBatchResult [ChangeMessageVisibilityBatchResultEntry]
cmvbrSuccessful = lens _cmvbrSuccessful (\s a -> s { _cmvbrSuccessful = a })

instance FromXML ChangeMessageVisibilityBatchResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ChangeMessageVisibilityBatchResult"

instance AWSRequest ChangeMessageVisibilityBatch where
    type Sv ChangeMessageVisibilityBatch = SQS
    type Rs ChangeMessageVisibilityBatch = ChangeMessageVisibilityBatchResult

    request  = post "ChangeMessageVisibilityBatch"
    response = xmlResponse $ \h x -> ChangeMessageVisibilityBatchResult
        <$> x %| "Failed"
        <*> x %| "Successful"
