{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.CreateEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift event notification subscription. This action
-- requires an ARN (Amazon Resource Name) of an Amazon SNS topic created by
-- either the Amazon Redshift console, the Amazon SNS console, or the Amazon
-- SNS API. To obtain an ARN with Amazon SNS, you must create a topic in
-- Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the source type, and lists of Amazon Redshift
-- source IDs, event categories, and event severities. Notifications will be
-- sent for all events you want that match those criteria. For example, you
-- can specify source type = cluster, source ID = my-cluster-1 and mycluster2,
-- event categories = Availability, Backup, and severity = ERROR. The
-- subscription will only send notifications for those ERROR events in the
-- Availability and Backup categories for the specified clusters. If you
-- specify both the source type and source IDs, such as source type = cluster
-- and source identifier = my-cluster-1, notifications will be sent for all
-- the cluster events for my-cluster-1. If you specify a source type but do
-- not specify a source identifier, you will receive notice of the events for
-- the objects of that type in your AWS account. If you do not specify either
-- the SourceType nor the SourceIdentifier, you will be notified of events
-- generated from all Amazon Redshift sources belonging to your AWS account.
-- You must specify a source type if you specify a source ID.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateEventSubscription.html>
module Network.AWS.Redshift.CreateEventSubscription
    (
    -- * Request
      CreateEventSubscription
    -- ** Request constructor
    , createEventSubscription
    -- ** Request lenses
    , cesEnabled
    , cesEventCategories
    , cesSeverity
    , cesSnsTopicArn
    , cesSourceIds
    , cesSourceType
    , cesSubscriptionName

    -- * Response
    , CreateEventSubscriptionResponse
    -- ** Response constructor
    , createEventSubscriptionResponse
    -- ** Response lenses
    , cesrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateEventSubscription = CreateEventSubscription
    { _cesEnabled          :: Maybe Bool
    , _cesEventCategories  :: [Text]
    , _cesSeverity         :: Maybe Text
    , _cesSnsTopicArn      :: Text
    , _cesSourceIds        :: [Text]
    , _cesSourceType       :: Maybe Text
    , _cesSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateEventSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'cesEventCategories' @::@ ['Text']
--
-- * 'cesSeverity' @::@ 'Maybe' 'Text'
--
-- * 'cesSnsTopicArn' @::@ 'Text'
--
-- * 'cesSourceIds' @::@ ['Text']
--
-- * 'cesSourceType' @::@ 'Maybe' 'Text'
--
-- * 'cesSubscriptionName' @::@ 'Text'
--
createEventSubscription :: Text -- ^ 'cesSubscriptionName'
                        -> Text -- ^ 'cesSnsTopicArn'
                        -> CreateEventSubscription
createEventSubscription p1 p2 = CreateEventSubscription
    { _cesSubscriptionName = p1
    , _cesSnsTopicArn      = p2
    , _cesSourceType       = Nothing
    , _cesSourceIds        = mempty
    , _cesEventCategories  = mempty
    , _cesSeverity         = Nothing
    , _cesEnabled          = Nothing
    }

-- | A Boolean value; set to true to activate the subscription, set to false
-- to create the subscription but not active it.
cesEnabled :: Lens' CreateEventSubscription (Maybe Bool)
cesEnabled = lens _cesEnabled (\s a -> s { _cesEnabled = a })

-- | Specifies the Amazon Redshift event categories to be published by the
-- event notification subscription. Values: Configuration, Management,
-- Monitoring, Security.
cesEventCategories :: Lens' CreateEventSubscription [Text]
cesEventCategories =
    lens _cesEventCategories (\s a -> s { _cesEventCategories = a })

-- | Specifies the Amazon Redshift event severity to be published by the event
-- notification subscription. Values: ERROR, INFO.
cesSeverity :: Lens' CreateEventSubscription (Maybe Text)
cesSeverity = lens _cesSeverity (\s a -> s { _cesSeverity = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used to transmit
-- the event notifications. The ARN is created by Amazon SNS when you create
-- a topic and subscribe to it.
cesSnsTopicArn :: Lens' CreateEventSubscription Text
cesSnsTopicArn = lens _cesSnsTopicArn (\s a -> s { _cesSnsTopicArn = a })

-- | A list of one or more identifiers of Amazon Redshift source objects. All
-- of the objects must be of the same type as was specified in the source
-- type parameter. The event subscription will return only events generated
-- by the specified objects. If not specified, then events are returned for
-- all objects within the source type specified. Example: my-cluster-1,
-- my-cluster-2 Example: my-snapshot-20131010.
cesSourceIds :: Lens' CreateEventSubscription [Text]
cesSourceIds = lens _cesSourceIds (\s a -> s { _cesSourceIds = a })

-- | The type of source that will be generating the events. For example, if
-- you want to be notified of events generated by a cluster, you would set
-- this parameter to cluster. If this value is not specified, events are
-- returned for all Amazon Redshift objects in your AWS account. You must
-- specify a source type in order to specify source IDs. Valid values:
-- cluster, cluster-parameter-group, cluster-security-group, and
-- cluster-snapshot.
cesSourceType :: Lens' CreateEventSubscription (Maybe Text)
cesSourceType = lens _cesSourceType (\s a -> s { _cesSourceType = a })

-- | The name of the event subscription to be created. Constraints: Cannot be
-- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
cesSubscriptionName :: Lens' CreateEventSubscription Text
cesSubscriptionName =
    lens _cesSubscriptionName (\s a -> s { _cesSubscriptionName = a })

newtype CreateEventSubscriptionResponse = CreateEventSubscriptionResponse
    { _cesrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

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

instance ToQuery CreateEventSubscription

instance ToHeaders CreateEventSubscription

instance AWSRequest CreateEventSubscription where
    type Sv CreateEventSubscription = Redshift
    type Rs CreateEventSubscription = CreateEventSubscriptionResponse

    request  = post "CreateEventSubscription"
    response = xmlResponse

instance FromXML CreateEventSubscriptionResponse where
    parseXML c = CreateEventSubscriptionResponse
        <$> c .:? "EventSubscription"
