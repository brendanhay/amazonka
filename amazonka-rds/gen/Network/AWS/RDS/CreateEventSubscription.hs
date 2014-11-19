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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateEventSubscription.html>
module Network.AWS.RDS.CreateEventSubscription
    (
    -- * Request
      CreateEventSubscription
    -- ** Request constructor
    , createEventSubscription
    -- ** Request lenses
    , cesEnabled
    , cesEventCategories
    , cesSnsTopicArn
    , cesSourceIds
    , cesSourceType
    , cesSubscriptionName
    , cesTags

    -- * Response
    , CreateEventSubscriptionResponse
    -- ** Response constructor
    , createEventSubscriptionResponse
    -- ** Response lenses
    , cesrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateEventSubscription = CreateEventSubscription
    { _cesEnabled          :: Maybe Bool
    , _cesEventCategories  :: List "EventCategory" Text
    , _cesSnsTopicArn      :: Text
    , _cesSourceIds        :: List "SourceId" Text
    , _cesSourceType       :: Maybe Text
    , _cesSubscriptionName :: Text
    , _cesTags             :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateEventSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'cesEventCategories' @::@ ['Text']
--
-- * 'cesSnsTopicArn' @::@ 'Text'
--
-- * 'cesSourceIds' @::@ ['Text']
--
-- * 'cesSourceType' @::@ 'Maybe' 'Text'
--
-- * 'cesSubscriptionName' @::@ 'Text'
--
-- * 'cesTags' @::@ ['Tag']
--
createEventSubscription :: Text -- ^ 'cesSubscriptionName'
                        -> Text -- ^ 'cesSnsTopicArn'
                        -> CreateEventSubscription
createEventSubscription p1 p2 = CreateEventSubscription
    { _cesSubscriptionName = p1
    , _cesSnsTopicArn      = p2
    , _cesSourceType       = Nothing
    , _cesEventCategories  = mempty
    , _cesSourceIds        = mempty
    , _cesEnabled          = Nothing
    , _cesTags             = mempty
    }

-- | A Boolean value; set to true to activate the subscription, set to false
-- to create the subscription but not active it.
cesEnabled :: Lens' CreateEventSubscription (Maybe Bool)
cesEnabled = lens _cesEnabled (\s a -> s { _cesEnabled = a })

-- | A list of event categories for a SourceType that you want to subscribe
-- to. You can see a list of the categories for a given SourceType in the
-- Events topic in the Amazon RDS User Guide or by using the
-- DescribeEventCategories action.
cesEventCategories :: Lens' CreateEventSubscription [Text]
cesEventCategories =
    lens _cesEventCategories (\s a -> s { _cesEventCategories = a })
        . _List

-- | The Amazon Resource Name (ARN) of the SNS topic created for event
-- notification. The ARN is created by Amazon SNS when you create a topic
-- and subscribe to it.
cesSnsTopicArn :: Lens' CreateEventSubscription Text
cesSnsTopicArn = lens _cesSnsTopicArn (\s a -> s { _cesSnsTopicArn = a })

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
cesSourceIds :: Lens' CreateEventSubscription [Text]
cesSourceIds = lens _cesSourceIds (\s a -> s { _cesSourceIds = a }) . _List

-- | The type of source that will be generating the events. For example, if
-- you want to be notified of events generated by a DB instance, you would
-- set this parameter to db-instance. if this value is not specified, all
-- events are returned. Valid values: db-instance | db-parameter-group |
-- db-security-group | db-snapshot.
cesSourceType :: Lens' CreateEventSubscription (Maybe Text)
cesSourceType = lens _cesSourceType (\s a -> s { _cesSourceType = a })

-- | The name of the subscription. Constraints: The name must be less than 255
-- characters.
cesSubscriptionName :: Lens' CreateEventSubscription Text
cesSubscriptionName =
    lens _cesSubscriptionName (\s a -> s { _cesSubscriptionName = a })

cesTags :: Lens' CreateEventSubscription [Tag]
cesTags = lens _cesTags (\s a -> s { _cesTags = a }) . _List

newtype CreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { _cesrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show)

-- | 'CreateEventSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
createEventSubscriptionResponse :: CreateEventSubscriptionResponse
createEventSubscriptionResponse = CreateEventSubscriptionResponse
    { _cesrEventSubscription = Nothing
    }

cesrEventSubscription :: Lens' CreateEventSubscriptionResponse (Maybe EventSubscription)
cesrEventSubscription =
    lens _cesrEventSubscription (\s a -> s { _cesrEventSubscription = a })

instance ToPath CreateEventSubscription where
    toPath = const "/"

instance ToQuery CreateEventSubscription where
    toQuery CreateEventSubscription{..} = mconcat
        [ "Enabled"          =? _cesEnabled
        , "EventCategories"  =? _cesEventCategories
        , "SnsTopicArn"      =? _cesSnsTopicArn
        , "SourceIds"        =? _cesSourceIds
        , "SourceType"       =? _cesSourceType
        , "SubscriptionName" =? _cesSubscriptionName
        , "Tags"             =? _cesTags
        ]

instance ToHeaders CreateEventSubscription

instance AWSRequest CreateEventSubscription where
    type Sv CreateEventSubscription = RDS
    type Rs CreateEventSubscription = CreateEventSubscriptionResponse

    request  = post "CreateEventSubscription"
    response = xmlResponse

instance FromXML CreateEventSubscriptionResponse where
    parseXML = withElement "CreateEventSubscriptionResult" $ \x -> CreateEventSubscriptionResponse
        <$> x .@? "EventSubscription"
