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
module Network.AWS.Redshift.CreateEventSubscription
    (
    -- * Request
      CreateEventSubscriptionMessage
    -- ** Request constructor
    , createEventSubscription
    -- ** Request lenses
    , cesmEnabled
    , cesmEventCategories
    , cesmSeverity
    , cesmSnsTopicArn
    , cesmSourceIds
    , cesmSourceType
    , cesmSubscriptionName

    -- * Response
    , CreateEventSubscriptionResult
    -- ** Response constructor
    , createEventSubscriptionResponse
    -- ** Response lenses
    , cesrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateEventSubscriptionMessage = CreateEventSubscriptionMessage
    { _cesmEnabled          :: Maybe Bool
    , _cesmEventCategories  :: [Text]
    , _cesmSeverity         :: Maybe Text
    , _cesmSnsTopicArn      :: Text
    , _cesmSourceIds        :: [Text]
    , _cesmSourceType       :: Maybe Text
    , _cesmSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateEventSubscriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesmEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'cesmEventCategories' @::@ ['Text']
--
-- * 'cesmSeverity' @::@ 'Maybe' 'Text'
--
-- * 'cesmSnsTopicArn' @::@ 'Text'
--
-- * 'cesmSourceIds' @::@ ['Text']
--
-- * 'cesmSourceType' @::@ 'Maybe' 'Text'
--
-- * 'cesmSubscriptionName' @::@ 'Text'
--
createEventSubscription :: Text -- ^ 'cesmSubscriptionName'
                        -> Text -- ^ 'cesmSnsTopicArn'
                        -> CreateEventSubscriptionMessage
createEventSubscription p1 p2 = CreateEventSubscriptionMessage
    { _cesmSubscriptionName = p1
    , _cesmSnsTopicArn      = p2
    , _cesmSourceType       = Nothing
    , _cesmSourceIds        = mempty
    , _cesmEventCategories  = mempty
    , _cesmSeverity         = Nothing
    , _cesmEnabled          = Nothing
    }

-- | A Boolean value; set to true to activate the subscription, set to false
-- to create the subscription but not active it.
cesmEnabled :: Lens' CreateEventSubscriptionMessage (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\s a -> s { _cesmEnabled = a })

-- | Specifies the Amazon Redshift event categories to be published by the
-- event notification subscription. Values: Configuration, Management,
-- Monitoring, Security.
cesmEventCategories :: Lens' CreateEventSubscriptionMessage [Text]
cesmEventCategories =
    lens _cesmEventCategories (\s a -> s { _cesmEventCategories = a })

-- | Specifies the Amazon Redshift event severity to be published by the event
-- notification subscription. Values: ERROR, INFO.
cesmSeverity :: Lens' CreateEventSubscriptionMessage (Maybe Text)
cesmSeverity = lens _cesmSeverity (\s a -> s { _cesmSeverity = a })

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic used to transmit
-- the event notifications. The ARN is created by Amazon SNS when you create
-- a topic and subscribe to it.
cesmSnsTopicArn :: Lens' CreateEventSubscriptionMessage Text
cesmSnsTopicArn = lens _cesmSnsTopicArn (\s a -> s { _cesmSnsTopicArn = a })

-- | A list of one or more identifiers of Amazon Redshift source objects. All
-- of the objects must be of the same type as was specified in the source
-- type parameter. The event subscription will return only events generated
-- by the specified objects. If not specified, then events are returned for
-- all objects within the source type specified. Example: my-cluster-1,
-- my-cluster-2 Example: my-snapshot-20131010.
cesmSourceIds :: Lens' CreateEventSubscriptionMessage [Text]
cesmSourceIds = lens _cesmSourceIds (\s a -> s { _cesmSourceIds = a })

-- | The type of source that will be generating the events. For example, if
-- you want to be notified of events generated by a cluster, you would set
-- this parameter to cluster. If this value is not specified, events are
-- returned for all Amazon Redshift objects in your AWS account. You must
-- specify a source type in order to specify source IDs. Valid values:
-- cluster, cluster-parameter-group, cluster-security-group, and
-- cluster-snapshot.
cesmSourceType :: Lens' CreateEventSubscriptionMessage (Maybe Text)
cesmSourceType = lens _cesmSourceType (\s a -> s { _cesmSourceType = a })

-- | The name of the event subscription to be created. Constraints: Cannot be
-- null, empty, or blank. Must contain from 1 to 255 alphanumeric characters
-- or hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
cesmSubscriptionName :: Lens' CreateEventSubscriptionMessage Text
cesmSubscriptionName =
    lens _cesmSubscriptionName (\s a -> s { _cesmSubscriptionName = a })

instance ToQuery CreateEventSubscriptionMessage

instance ToPath CreateEventSubscriptionMessage where
    toPath = const "/"

newtype CreateEventSubscriptionResult = CreateEventSubscriptionResult
    { _cesrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'CreateEventSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
createEventSubscriptionResponse :: CreateEventSubscriptionResult
createEventSubscriptionResponse = CreateEventSubscriptionResult
    { _cesrEventSubscription = Nothing
    }

cesrEventSubscription :: Lens' CreateEventSubscriptionResult (Maybe EventSubscription)
cesrEventSubscription =
    lens _cesrEventSubscription (\s a -> s { _cesrEventSubscription = a })

instance FromXML CreateEventSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateEventSubscriptionResult"

instance AWSRequest CreateEventSubscriptionMessage where
    type Sv CreateEventSubscriptionMessage = Redshift
    type Rs CreateEventSubscriptionMessage = CreateEventSubscriptionResult

    request  = post "CreateEventSubscription"
    response = xmlResponse $ \h x -> CreateEventSubscriptionResult
        <$> x %| "EventSubscription"
