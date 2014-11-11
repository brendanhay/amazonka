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

-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and/or delivery notifications for emails sent with that
-- identity as the Source. This action is throttled at one request per second.
-- For more information about feedback notification, see the Amazon SES
-- Developer Guide.
module Network.AWS.SES.SetIdentityNotificationTopic
    (
    -- * Request
      SetIdentityNotificationTopic
    -- ** Request constructor
    , setIdentityNotificationTopic
    -- ** Request lenses
    , sintIdentity
    , sintNotificationType
    , sintSnsTopic

    -- * Response
    , SetIdentityNotificationTopicResponse
    -- ** Response constructor
    , setIdentityNotificationTopicResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SES.Types

data SetIdentityNotificationTopic = SetIdentityNotificationTopic
    { _sintIdentity         :: Text
    , _sintNotificationType :: Text
    , _sintSnsTopic         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetIdentityNotificationTopic' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sintIdentity' @::@ 'Text'
--
-- * 'sintNotificationType' @::@ 'Text'
--
-- * 'sintSnsTopic' @::@ 'Maybe' 'Text'
--
setIdentityNotificationTopic :: Text -- ^ 'sintIdentity'
                             -> Text -- ^ 'sintNotificationType'
                             -> SetIdentityNotificationTopic
setIdentityNotificationTopic p1 p2 = SetIdentityNotificationTopic
    { _sintIdentity         = p1
    , _sintNotificationType = p2
    , _sintSnsTopic         = Nothing
    }

-- | The identity for which the Amazon SNS topic will be set. Examples:
-- user@example.com, example.com.
sintIdentity :: Lens' SetIdentityNotificationTopic Text
sintIdentity = lens _sintIdentity (\s a -> s { _sintIdentity = a })

-- | The type of notifications that will be published to the specified Amazon
-- SNS topic.
sintNotificationType :: Lens' SetIdentityNotificationTopic Text
sintNotificationType =
    lens _sintNotificationType (\s a -> s { _sintNotificationType = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
-- is omitted from the request or a null value is passed, SnsTopic is
-- cleared and publishing is disabled.
sintSnsTopic :: Lens' SetIdentityNotificationTopic (Maybe Text)
sintSnsTopic = lens _sintSnsTopic (\s a -> s { _sintSnsTopic = a })
instance ToQuery SetIdentityNotificationTopic

instance ToPath SetIdentityNotificationTopic where
    toPath = const "/"

data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetIdentityNotificationTopicResponse' constructor.
setIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse
setIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
instance FromXML SetIdentityNotificationTopicResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetIdentityNotificationTopicResponse"

instance AWSRequest SetIdentityNotificationTopic where
    type Sv SetIdentityNotificationTopic = SES
    type Rs SetIdentityNotificationTopic = SetIdentityNotificationTopicResponse

    request  = post "SetIdentityNotificationTopic"
    response = nullaryResponse SetIdentityNotificationTopicResponse
