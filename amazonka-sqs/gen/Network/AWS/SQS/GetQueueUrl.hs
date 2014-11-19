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

-- Module      : Network.AWS.SQS.GetQueueUrl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the URL of an existing queue. This action provides a simple way to
-- retrieve the URL of an Amazon SQS queue. To access a queue that belongs to
-- another AWS account, use the QueueOwnerAWSAccountId parameter to specify
-- the account ID of the queue's owner. The queue's owner must grant you
-- permission to access the queue. For more information about shared queue
-- access, see AddPermission or go to Shared Queues in the Amazon SQS
-- Developer Guide.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_GetQueueUrl.html>
module Network.AWS.SQS.GetQueueUrl
    (
    -- * Request
      GetQueueUrl
    -- ** Request constructor
    , getQueueUrl
    -- ** Request lenses
    , gquQueueName
    , gquQueueOwnerAWSAccountId

    -- * Response
    , GetQueueUrlResponse
    -- ** Response constructor
    , getQueueUrlResponse
    -- ** Response lenses
    , gqurQueueUrl
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data GetQueueUrl = GetQueueUrl
    { _gquQueueName              :: Text
    , _gquQueueOwnerAWSAccountId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetQueueUrl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gquQueueName' @::@ 'Text'
--
-- * 'gquQueueOwnerAWSAccountId' @::@ 'Maybe' 'Text'
--
getQueueUrl :: Text -- ^ 'gquQueueName'
            -> GetQueueUrl
getQueueUrl p1 = GetQueueUrl
    { _gquQueueName              = p1
    , _gquQueueOwnerAWSAccountId = Nothing
    }

-- | The name of the queue whose URL must be fetched. Maximum 80 characters;
-- alphanumeric characters, hyphens (-), and underscores (_) are allowed.
gquQueueName :: Lens' GetQueueUrl Text
gquQueueName = lens _gquQueueName (\s a -> s { _gquQueueName = a })

-- | The AWS account ID of the account that created the queue.
gquQueueOwnerAWSAccountId :: Lens' GetQueueUrl (Maybe Text)
gquQueueOwnerAWSAccountId =
    lens _gquQueueOwnerAWSAccountId
        (\s a -> s { _gquQueueOwnerAWSAccountId = a })

newtype GetQueueUrlResponse = GetQueueUrlResponse
    { _gqurQueueUrl :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetQueueUrlResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gqurQueueUrl' @::@ 'Maybe' 'Text'
--
getQueueUrlResponse :: GetQueueUrlResponse
getQueueUrlResponse = GetQueueUrlResponse
    { _gqurQueueUrl = Nothing
    }

-- | The URL for the queue.
gqurQueueUrl :: Lens' GetQueueUrlResponse (Maybe Text)
gqurQueueUrl = lens _gqurQueueUrl (\s a -> s { _gqurQueueUrl = a })

instance ToPath GetQueueUrl where
    toPath = const "/"

instance ToQuery GetQueueUrl where
    toQuery GetQueueUrl{..} = mconcat
        [ "QueueName" =? _gquQueueName
        , "QueueOwnerAWSAccountId" =? _gquQueueOwnerAWSAccountId
        ]

instance ToHeaders GetQueueUrl

instance AWSRequest GetQueueUrl where
    type Sv GetQueueUrl = SQS
    type Rs GetQueueUrl = GetQueueUrlResponse

    request  = post "GetQueueUrl"
    response = xmlResponse

instance FromXML GetQueueUrlResponse where
    parseXML = withElement "GetQueueUrlResult" $ \x -> GetQueueUrlResponse
        <$> x .@? "QueueUrl"
