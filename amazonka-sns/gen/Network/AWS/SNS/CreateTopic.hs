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

-- Module      : Network.AWS.SNS.CreateTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a topic to which notifications can be published. Users can create
-- at most 3000 topics. For more information, see http://aws.amazon.com/sns.
-- This action is idempotent, so if the requester already owns a topic with
-- the specified name, that topic's ARN is returned without creating a new
-- topic.
module Network.AWS.SNS.CreateTopic
    (
    -- * Request
      CreateTopicInput
    -- ** Request constructor
    , createTopic
    -- ** Request lenses
    , ctiName

    -- * Response
    , CreateTopicResponse
    -- ** Response constructor
    , createTopicResponse
    -- ** Response lenses
    , ctrTopicArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype CreateTopicInput = CreateTopicInput
    { _ctiName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'CreateTopicInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctiName' @::@ 'Text'
--
createTopic :: Text -- ^ 'ctiName'
            -> CreateTopicInput
createTopic p1 = CreateTopicInput
    { _ctiName = p1
    }

-- | The name of the topic you want to create. Constraints: Topic names must
-- be made up of only uppercase and lowercase ASCII letters, numbers,
-- underscores, and hyphens, and must be between 1 and 256 characters long.
ctiName :: Lens' CreateTopicInput Text
ctiName = lens _ctiName (\s a -> s { _ctiName = a })

instance ToQuery CreateTopicInput

instance ToPath CreateTopicInput where
    toPath = const "/"

newtype CreateTopicResponse = CreateTopicResponse
    { _ctrTopicArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreateTopicResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctrTopicArn' @::@ 'Maybe' 'Text'
--
createTopicResponse :: CreateTopicResponse
createTopicResponse = CreateTopicResponse
    { _ctrTopicArn = Nothing
    }

-- | The Amazon Resource Name (ARN) assigned to the created topic.
ctrTopicArn :: Lens' CreateTopicResponse (Maybe Text)
ctrTopicArn = lens _ctrTopicArn (\s a -> s { _ctrTopicArn = a })

instance FromXML CreateTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateTopicResponse"

instance AWSRequest CreateTopicInput where
    type Sv CreateTopicInput = SNS
    type Rs CreateTopicInput = CreateTopicResponse

    request  = post "CreateTopic"
    response = xmlResponse $ \h x -> CreateTopicResponse
        <$> x %| "TopicArn"
