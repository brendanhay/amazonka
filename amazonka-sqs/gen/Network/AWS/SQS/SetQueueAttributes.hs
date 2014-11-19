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

-- Module      : Network.AWS.SQS.SetQueueAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the value of one or more queue attributes. When you change a queue's
-- attributes, the change can take up to 60 seconds for most of the attributes
-- to propagate throughout the SQS system. Changes made to the
-- MessageRetentionPeriod attribute can take up to 15 minutes.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SetQueueAttributes.html>
module Network.AWS.SQS.SetQueueAttributes
    (
    -- * Request
      SetQueueAttributes
    -- ** Request constructor
    , setQueueAttributes
    -- ** Request lenses
    , sqaAttributes
    , sqaQueueUrl

    -- * Response
    , SetQueueAttributesResponse
    -- ** Response constructor
    , setQueueAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data SetQueueAttributes = SetQueueAttributes
    { _sqaAttributes :: Map "Attribute" "Name" Text "Value" Text
    , _sqaQueueUrl   :: Text
    } deriving (Eq, Show, Generic)

-- | 'SetQueueAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sqaAttributes' @::@ ('HashMap' 'Text' 'Text')
--
-- * 'sqaQueueUrl' @::@ 'Text'
--
setQueueAttributes :: Text -- ^ 'sqaQueueUrl'
                   -> (HashMap Text Text) -- ^ 'sqaAttributes'
                   -> SetQueueAttributes
setQueueAttributes p1 p2 = SetQueueAttributes
    { _sqaQueueUrl   = p1
    , _sqaAttributes = withIso _Map (const id) p2
    }

-- | A map of attributes to set. The following lists the names, descriptions,
-- and values of the special request parameters the SetQueueAttributes
-- action uses: DelaySeconds - The time in seconds that the delivery of all
-- messages in the queue will be delayed. An integer from 0 to 900 (15
-- minutes). The default for this attribute is 0 (zero). MaximumMessageSize
-- - The limit of how many bytes a message can contain before Amazon SQS
-- rejects it. An integer from 1024 bytes (1 KiB) up to 262144 bytes (256
-- KiB). The default for this attribute is 262144 (256 KiB).
-- MessageRetentionPeriod - The number of seconds Amazon SQS retains a
-- message. Integer representing seconds, from 60 (1 minute) to 1209600 (14
-- days). The default for this attribute is 345600 (4 days). Policy - The
-- queue's policy. A valid form-url-encoded policy. For more information
-- about policy structure, see Basic Policy Structure in the Amazon SQS
-- Developer Guide. For more information about form-url-encoding, see
-- http://www.w3.org/MarkUp/html-spec/html-spec_8.html#SEC8.2.1.
-- ReceiveMessageWaitTimeSeconds - The time for which a ReceiveMessage call
-- will wait for a message to arrive. An integer from 0 to 20 (seconds). The
-- default for this attribute is 0. VisibilityTimeout - The visibility
-- timeout for the queue. An integer from 0 to 43200 (12 hours). The default
-- for this attribute is 30. For more information about visibility timeout,
-- see Visibility Timeout in the Amazon SQS Developer Guide. RedrivePolicy -
-- The parameters for dead letter queue functionality of the source queue.
-- For more information about RedrivePolicy and dead letter queues, see
-- Using Amazon SQS Dead Letter Queues in the Amazon SQS Developer Guide.
sqaAttributes :: Lens' SetQueueAttributes ((HashMap Text Text))
sqaAttributes = lens _sqaAttributes (\s a -> s { _sqaAttributes = a })
    . _Map . _Map

-- | The URL of the Amazon SQS queue to take action on.
sqaQueueUrl :: Lens' SetQueueAttributes Text
sqaQueueUrl = lens _sqaQueueUrl (\s a -> s { _sqaQueueUrl = a })

data SetQueueAttributesResponse = SetQueueAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetQueueAttributesResponse' constructor.
setQueueAttributesResponse :: SetQueueAttributesResponse
setQueueAttributesResponse = SetQueueAttributesResponse

instance ToPath SetQueueAttributes where
    toPath = const "/"

instance ToQuery SetQueueAttributes

instance ToHeaders SetQueueAttributes

instance AWSRequest SetQueueAttributes where
    type Sv SetQueueAttributes = SQS
    type Rs SetQueueAttributes = SetQueueAttributesResponse

    request  = post "SetQueueAttributes"
    response = nullResponse SetQueueAttributesResponse
