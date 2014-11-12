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

-- Module      : Network.AWS.RDS.CreateEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an RDS event notification subscription. This action requires a
-- topic ARN (Amazon Resource Name) created by either the RDS console, the SNS
-- console, or the SNS API. To obtain an ARN with SNS, you must create a topic
-- in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the type of source (SourceType) you want to be
-- notified of, provide a list of RDS sources (SourceIds) that triggers the
-- events, and provide a list of event categories (EventCategories) for events
-- you want to be notified of. For example, you can specify SourceType =
-- db-instance, SourceIds = mydbinstance1, mydbinstance2 and EventCategories =
-- Availability, Backup. If you specify both the SourceType and SourceIds,
-- such as SourceType = db-instance and SourceIdentifier = myDBInstance1, you
-- will be notified of all the db-instance events for the specified source. If
-- you specify a SourceType but do not specify a SourceIdentifier, you will
-- receive notice of the events for that source type for all your RDS sources.
-- If you do not specify either the SourceType nor the SourceIdentifier, you
-- will be notified of events generated from all RDS sources belonging to your
-- customer account.
module Network.AWS.RDS.CreateEventSubscription
    (
    -- * Request
      CreateEventSubscriptionMessage
    -- ** Request constructor
    , createEventSubscriptionMessage
    -- ** Request lenses
    , cesmEnabled
    , cesmEventCategories
    , cesmSnsTopicArn
    , cesmSourceIds
    , cesmSourceType
    , cesmSubscriptionName
    , cesmTags

    -- * Response
    , CreateEventSubscriptionResult
    -- ** Response constructor
    , createEventSubscriptionResult
    -- ** Response lenses
    , cesrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateEventSubscriptionMessage = CreateEventSubscriptionMessage
    { _cesmEnabled          :: Maybe Bool
    , _cesmEventCategories  :: [Text]
    , _cesmSnsTopicArn      :: Text
    , _cesmSourceIds        :: [Text]
    , _cesmSourceType       :: Maybe Text
    , _cesmSubscriptionName :: Text
    , _cesmTags             :: [Tag]
    } (Eq, Show, Generic)

-- | 'CreateEventSubscriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesmEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'cesmEventCategories' @::@ ['Text']
--
-- * 'cesmSnsTopicArn' @::@ 'Text'
--
-- * 'cesmSourceIds' @::@ ['Text']
--
-- * 'cesmSourceType' @::@ 'Maybe' 'Text'
--
-- * 'cesmSubscriptionName' @::@ 'Text'
--
-- * 'cesmTags' @::@ ['Tag']
--
createEventSubscriptionMessage :: Text -- ^ 'cesmSubscriptionName'
                               -> Text -- ^ 'cesmSnsTopicArn'
                               -> CreateEventSubscriptionMessage
createEventSubscriptionMessage p1 p2 = CreateEventSubscriptionMessage
    { _cesmSubscriptionName = p1
    , _cesmSnsTopicArn      = p2
    , _cesmSourceType       = Nothing
    , _cesmEventCategories  = mempty
    , _cesmSourceIds        = mempty
    , _cesmEnabled          = Nothing
    , _cesmTags             = mempty
    }

-- | A Boolean value; set to true to activate the subscription, set to false
-- to create the subscription but not active it.
cesmEnabled :: Lens' CreateEventSubscriptionMessage (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\s a -> s { _cesmEnabled = a })

-- | A list of event categories for a SourceType that you want to subscribe
-- to. You can see a list of the categories for a given SourceType in the
-- Events topic in the Amazon RDS User Guide or by using the
-- DescribeEventCategories action.
cesmEventCategories :: Lens' CreateEventSubscriptionMessage [Text]
cesmEventCategories =
    lens _cesmEventCategories (\s a -> s { _cesmEventCategories = a })

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
cesmSnsTopicArn :: Lens' CreateEventSubscriptionMessage Text
cesmSnsTopicArn = lens _cesmSnsTopicArn (\s a -> s { _cesmSnsTopicArn = a })

-- | The list of identifiers of the event sources for which events will be
-- returned. If not specified, then all sources are included in the
-- response. An identifier must begin with a letter and must contain only
-- ASCII letters, digits, and hyphens; it cannot end with a hyphen or
-- contain two consecutive hyphens. Constraints: If SourceIds are supplied,
-- SourceType must also be provided. If the source type is a DB instance,
-- then a DBInstanceIdentifier must be supplied. If the source type is a DB
-- security group, a DBSecurityGroupName must be supplied. If the source
-- type is a DB parameter group, a DBParameterGroupName must be supplied. If
-- the source type is a DB snapshot, a DBSnapshotIdentifier must be
-- supplied.
cesmSourceIds :: Lens' CreateEventSubscriptionMessage [Text]
cesmSourceIds = lens _cesmSourceIds (\s a -> s { _cesmSourceIds = a })

-- | The type of source that will be generating the events. For example, if
-- you want to be notified of events generated by a DB instance, you would
-- set this parameter to db-instance. if this value is not specified, all
-- events are returned. Valid values: db-instance | db-parameter-group |
-- db-security-group | db-snapshot.
cesmSourceType :: Lens' CreateEventSubscriptionMessage (Maybe Text)
cesmSourceType = lens _cesmSourceType (\s a -> s { _cesmSourceType = a })

-- | The name of the subscription. Constraints: The name must be less than 255
-- characters.
cesmSubscriptionName :: Lens' CreateEventSubscriptionMessage Text
cesmSubscriptionName =
    lens _cesmSubscriptionName (\s a -> s { _cesmSubscriptionName = a })

cesmTags :: Lens' CreateEventSubscriptionMessage [Tag]
cesmTags = lens _cesmTags (\s a -> s { _cesmTags = a })
instance ToQuery CreateEventSubscriptionMessage

instance ToPath CreateEventSubscriptionMessage where
    toPath = const "/"

newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult
    { _cesrEventSubscription :: Maybe EventSubscription
    } (Eq, Show, Generic)

-- | 'CreateEventSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
createEventSubscriptionResult :: CreateEventSubscriptionResult
createEventSubscriptionResult = CreateEventSubscriptionResult
    { _cesrEventSubscription = Nothing
    }

cesrEventSubscription :: Lens' CreateEventSubscriptionResult (Maybe EventSubscription)
cesrEventSubscription =
    lens _cesrEventSubscription (\s a -> s { _cesrEventSubscription = a })

instance FromXML CreateEventSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateEventSubscriptionResult"

instance AWSRequest CreateEventSubscriptionMessage where
    type Sv CreateEventSubscriptionMessage = RDS
    type Rs CreateEventSubscriptionMessage = CreateEventSubscriptionResult

    request  = post "CreateEventSubscription"
    response = xmlResponse $ \h x -> CreateEventSubscriptionResult
        <$> x %| "EventSubscription"
