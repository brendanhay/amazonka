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

-- Module      : Network.AWS.SES.SetIdentityNotificationTopic
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

-- | Given an identity (email address or domain), sets the Amazon Simple
-- Notification Service (Amazon SNS) topic to which Amazon SES will publish
-- bounce, complaint, and/or delivery notifications for emails sent with that
-- identity as the 'Source'.
--
-- This action is throttled at one request per second.
--
-- For more information about feedback notification, see the <http://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SESDeveloper Guide>.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_SetIdentityNotificationTopic.html>
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
import qualified GHC.Exts

data SetIdentityNotificationTopic = SetIdentityNotificationTopic
    { _sintIdentity         :: Text
    , _sintNotificationType :: NotificationType
    , _sintSnsTopic         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'SetIdentityNotificationTopic' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sintIdentity' @::@ 'Text'
--
-- * 'sintNotificationType' @::@ 'NotificationType'
--
-- * 'sintSnsTopic' @::@ 'Maybe' 'Text'
--
setIdentityNotificationTopic :: Text -- ^ 'sintIdentity'
                             -> NotificationType -- ^ 'sintNotificationType'
                             -> SetIdentityNotificationTopic
setIdentityNotificationTopic p1 p2 = SetIdentityNotificationTopic
    { _sintIdentity         = p1
    , _sintNotificationType = p2
    , _sintSnsTopic         = Nothing
    }

-- | The identity for which the Amazon SNS topic will be set. Examples: 'user@example.com', 'example.com'.
sintIdentity :: Lens' SetIdentityNotificationTopic Text
sintIdentity = lens _sintIdentity (\s a -> s { _sintIdentity = a })

-- | The type of notifications that will be published to the specified Amazon SNS
-- topic.
sintNotificationType :: Lens' SetIdentityNotificationTopic NotificationType
sintNotificationType =
    lens _sintNotificationType (\s a -> s { _sintNotificationType = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter is
-- omitted from the request or a null value is passed, 'SnsTopic' is cleared and
-- publishing is disabled.
sintSnsTopic :: Lens' SetIdentityNotificationTopic (Maybe Text)
sintSnsTopic = lens _sintSnsTopic (\s a -> s { _sintSnsTopic = a })

data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetIdentityNotificationTopicResponse' constructor.
setIdentityNotificationTopicResponse :: SetIdentityNotificationTopicResponse
setIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse

instance ToPath SetIdentityNotificationTopic where
    toPath = const "/"

instance ToQuery SetIdentityNotificationTopic where
    toQuery SetIdentityNotificationTopic{..} = mconcat
        [ "Identity"         =? _sintIdentity
        , "NotificationType" =? _sintNotificationType
        , "SnsTopic"         =? _sintSnsTopic
        ]

instance ToHeaders SetIdentityNotificationTopic

instance AWSRequest SetIdentityNotificationTopic where
    type Sv SetIdentityNotificationTopic = SES
    type Rs SetIdentityNotificationTopic = SetIdentityNotificationTopicResponse

    request  = post "SetIdentityNotificationTopic"
    response = nullResponse SetIdentityNotificationTopicResponse
