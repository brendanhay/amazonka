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

-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a source identifier from an existing RDS event notification
-- subscription.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
    (
    -- * Request
      RemoveSourceIdentifierFromSubscriptionMessage
    -- ** Request constructor
    , removeSourceIdentifierFromSubscriptionMessage
    -- ** Request lenses
    , rsifsmSourceIdentifier
    , rsifsmSubscriptionName

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResult
    -- ** Response constructor
    , removeSourceIdentifierFromSubscriptionResult
    -- ** Response lenses
    , rsifsrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RemoveSourceIdentifierFromSubscriptionMessage = RemoveSourceIdentifierFromSubscriptionMessage
    { _rsifsmSourceIdentifier :: Text
    , _rsifsmSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsmSourceIdentifier' @::@ 'Text'
--
-- * 'rsifsmSubscriptionName' @::@ 'Text'
--
removeSourceIdentifierFromSubscriptionMessage :: Text -- ^ 'rsifsmSubscriptionName'
                                              -> Text -- ^ 'rsifsmSourceIdentifier'
                                              -> RemoveSourceIdentifierFromSubscriptionMessage
removeSourceIdentifierFromSubscriptionMessage p1 p2 = RemoveSourceIdentifierFromSubscriptionMessage
    { _rsifsmSubscriptionName = p1
    , _rsifsmSourceIdentifier = p2
    }

-- | The source identifier to be removed from the subscription, such as the DB
-- instance identifier for a DB instance or the name of a security group.
rsifsmSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscriptionMessage Text
rsifsmSourceIdentifier =
    lens _rsifsmSourceIdentifier (\s a -> s { _rsifsmSourceIdentifier = a })

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsmSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscriptionMessage Text
rsifsmSubscriptionName =
    lens _rsifsmSubscriptionName (\s a -> s { _rsifsmSubscriptionName = a })
instance ToQuery RemoveSourceIdentifierFromSubscriptionMessage

instance ToPath RemoveSourceIdentifierFromSubscriptionMessage where
    toPath = const "/"

newtype RemoveSourceIdentifierFromSubscriptionResult = RemoveSourceIdentifierFromSubscriptionResult
    { _rsifsrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
removeSourceIdentifierFromSubscriptionResult :: RemoveSourceIdentifierFromSubscriptionResult
removeSourceIdentifierFromSubscriptionResult = RemoveSourceIdentifierFromSubscriptionResult
    { _rsifsrEventSubscription = Nothing
    }

rsifsrEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResult (Maybe EventSubscription)
rsifsrEventSubscription =
    lens _rsifsrEventSubscription (\s a -> s { _rsifsrEventSubscription = a })
instance FromXML RemoveSourceIdentifierFromSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveSourceIdentifierFromSubscriptionResult"

instance AWSRequest RemoveSourceIdentifierFromSubscriptionMessage where
    type Sv RemoveSourceIdentifierFromSubscriptionMessage = RDS
    type Rs RemoveSourceIdentifierFromSubscriptionMessage = RemoveSourceIdentifierFromSubscriptionResult

    request  = post "RemoveSourceIdentifierFromSubscription"
    response = xmlResponse $ \h x -> RemoveSourceIdentifierFromSubscriptionResult
        <$> x %| "EventSubscription"
