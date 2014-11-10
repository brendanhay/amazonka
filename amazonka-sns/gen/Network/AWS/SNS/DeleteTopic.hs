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
module Network.AWS.SNS.DeleteTopic
    (
    -- * Request
      DeleteTopicInput
    -- ** Request constructor
    , deleteTopic
    -- ** Request lenses
    , dtiTopicArn

    -- * Response
    , DeleteTopicResponse
    -- ** Response constructor
    , deleteTopicResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype DeleteTopicInput = DeleteTopicInput
    { _dtiTopicArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteTopicInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtiTopicArn' @::@ 'Text'
--
deleteTopic :: Text -- ^ 'dtiTopicArn'
            -> DeleteTopicInput
deleteTopic p1 = DeleteTopicInput
    { _dtiTopicArn = p1
    }

-- | The ARN of the topic you want to delete.
dtiTopicArn :: Lens' DeleteTopicInput Text
dtiTopicArn = lens _dtiTopicArn (\s a -> s { _dtiTopicArn = a })

instance ToPath DeleteTopicInput where
    toPath = const "/"

instance ToQuery DeleteTopicInput

data DeleteTopicResponse = DeleteTopicResponse

-- | 'DeleteTopicResponse' constructor.
deleteTopicResponse :: DeleteTopicResponse
deleteTopicResponse = DeleteTopicResponse

instance AWSRequest DeleteTopicInput where
    type Sv DeleteTopicInput = SNS
    type Rs DeleteTopicInput = DeleteTopicResponse

    request  = post "DeleteTopic"
    response = const (nullaryResponse DeleteTopicResponse)
