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

-- Module      : Network.AWS.SNS.DeleteTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a topic and all its subscriptions. Deleting a topic might prevent
-- some messages previously sent to the topic from being delivered to
-- subscribers. This action is idempotent, so deleting a topic that does not
-- exist does not result in an error.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_DeleteTopic.html>
module Network.AWS.SNS.DeleteTopic
    (
    -- * Request
      DeleteTopic
    -- ** Request constructor
    , deleteTopic
    -- ** Request lenses
    , dtTopicArn

    -- * Response
    , DeleteTopicResponse
    -- ** Response constructor
    , deleteTopicResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

newtype DeleteTopic = DeleteTopic
    { _dtTopicArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteTopic' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtTopicArn' @::@ 'Text'
--
deleteTopic :: Text -- ^ 'dtTopicArn'
            -> DeleteTopic
deleteTopic p1 = DeleteTopic
    { _dtTopicArn = p1
    }

-- | The ARN of the topic you want to delete.
dtTopicArn :: Lens' DeleteTopic Text
dtTopicArn = lens _dtTopicArn (\s a -> s { _dtTopicArn = a })

data DeleteTopicResponse = DeleteTopicResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteTopicResponse' constructor.
deleteTopicResponse :: DeleteTopicResponse
deleteTopicResponse = DeleteTopicResponse

instance ToPath DeleteTopic where
    toPath = const "/"

instance ToQuery DeleteTopic where
    toQuery DeleteTopic{..} = mconcat
        [ "TopicArn" =? _dtTopicArn
        ]

instance ToHeaders DeleteTopic

query

instance AWSRequest DeleteTopic where
    type Sv DeleteTopic = SNS
    type Rs DeleteTopic = DeleteTopicResponse

    request  = post "DeleteTopic"
    response = nullResponse DeleteTopicResponse
