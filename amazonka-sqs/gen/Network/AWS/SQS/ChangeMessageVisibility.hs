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

-- Module      : Network.AWS.SQS.ChangeMessageVisibility
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the visibility timeout of a specified message in a queue to a new
-- value. The maximum allowed timeout value you can set the value to is 12
-- hours. This means you can't extend the timeout of a message in an existing
-- queue to more than a total visibility timeout of 12 hours. (For more
-- information visibility timeout, see Visibility Timeout in the Amazon SQS
-- Developer Guide.) For example, let's say you have a message and its default
-- message visibility timeout is 30 minutes. You could call
-- ChangeMessageVisiblity with a value of two hours and the effective timeout
-- would be two hours and 30 minutes. When that time comes near you could
-- again extend the time out by calling ChangeMessageVisiblity, but this time
-- the maximum allowed timeout would be 9 hours and 30 minutes. If you attempt
-- to set the VisibilityTimeout to an amount more than the maximum time left,
-- Amazon SQS returns an error. It will not automatically recalculate and
-- increase the timeout to the maximum time remaining. Unlike with a queue,
-- when you change the visibility timeout for a specific message, that timeout
-- value is applied immediately but is not saved in memory for that message.
-- If you don't delete a message after it is received, the visibility timeout
-- for the message the next time it is received reverts to the original
-- timeout value, not the value you set with the ChangeMessageVisibility
-- action.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ChangeMessageVisibility.html>
module Network.AWS.SQS.ChangeMessageVisibility
    (
    -- * Request
      ChangeMessageVisibility
    -- ** Request constructor
    , changeMessageVisibility
    -- ** Request lenses
    , cmvQueueUrl
    , cmvReceiptHandle
    , cmvVisibilityTimeout

    -- * Response
    , ChangeMessageVisibilityResponse
    -- ** Response constructor
    , changeMessageVisibilityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data ChangeMessageVisibility = ChangeMessageVisibility
    { _cmvQueueUrl          :: Text
    , _cmvReceiptHandle     :: Text
    , _cmvVisibilityTimeout :: Int
    } deriving (Eq, Ord, Show)

-- | 'ChangeMessageVisibility' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmvQueueUrl' @::@ 'Text'
--
-- * 'cmvReceiptHandle' @::@ 'Text'
--
-- * 'cmvVisibilityTimeout' @::@ 'Int'
--
changeMessageVisibility :: Text -- ^ 'cmvQueueUrl'
                        -> Text -- ^ 'cmvReceiptHandle'
                        -> Int -- ^ 'cmvVisibilityTimeout'
                        -> ChangeMessageVisibility
changeMessageVisibility p1 p2 p3 = ChangeMessageVisibility
    { _cmvQueueUrl          = p1
    , _cmvReceiptHandle     = p2
    , _cmvVisibilityTimeout = p3
    }

-- | The URL of the Amazon SQS queue to take action on.
cmvQueueUrl :: Lens' ChangeMessageVisibility Text
cmvQueueUrl = lens _cmvQueueUrl (\s a -> s { _cmvQueueUrl = a })

-- | The receipt handle associated with the message whose visibility timeout
-- should be changed. This parameter is returned by the ReceiveMessage
-- action.
cmvReceiptHandle :: Lens' ChangeMessageVisibility Text
cmvReceiptHandle = lens _cmvReceiptHandle (\s a -> s { _cmvReceiptHandle = a })

-- | The new value (in seconds - from 0 to 43200 - maximum 12 hours) for the
-- message's visibility timeout.
cmvVisibilityTimeout :: Lens' ChangeMessageVisibility Int
cmvVisibilityTimeout =
    lens _cmvVisibilityTimeout (\s a -> s { _cmvVisibilityTimeout = a })

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ChangeMessageVisibilityResponse' constructor.
changeMessageVisibilityResponse :: ChangeMessageVisibilityResponse
changeMessageVisibilityResponse = ChangeMessageVisibilityResponse

instance ToPath ChangeMessageVisibility where
    toPath = const "/"

instance ToQuery ChangeMessageVisibility where
    toQuery ChangeMessageVisibility{..} = mconcat
        [ "QueueUrl"          =? _cmvQueueUrl
        , "ReceiptHandle"     =? _cmvReceiptHandle
        , "VisibilityTimeout" =? _cmvVisibilityTimeout
        ]

instance ToHeaders ChangeMessageVisibility

instance AWSRequest ChangeMessageVisibility where
    type Sv ChangeMessageVisibility = SQS
    type Rs ChangeMessageVisibility = ChangeMessageVisibilityResponse

    request  = post "ChangeMessageVisibility"
    response = nullResponse ChangeMessageVisibilityResponse


Some kind of operator / class to check the types whether to continue?
