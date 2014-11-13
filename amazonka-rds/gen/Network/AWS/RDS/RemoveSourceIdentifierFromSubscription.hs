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
      RemoveSourceIdentifierFromSubscription
    -- ** Request constructor
    , removeSourceIdentifierFromSubscription
    -- ** Request lenses
    , rsifsSourceIdentifier
    , rsifsSubscriptionName

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResponse
    -- ** Response constructor
    , removeSourceIdentifierFromSubscriptionResponse
    -- ** Response lenses
    , rsifsrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { _rsifsSourceIdentifier :: Text
    , _rsifsSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsSourceIdentifier' @::@ 'Text'
--
-- * 'rsifsSubscriptionName' @::@ 'Text'
--
removeSourceIdentifierFromSubscription :: Text -- ^ 'rsifsSubscriptionName'
                                       -> Text -- ^ 'rsifsSourceIdentifier'
                                       -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription p1 p2 = RemoveSourceIdentifierFromSubscription
    { _rsifsSubscriptionName = p1
    , _rsifsSourceIdentifier = p2
    }

-- | The source identifier to be removed from the subscription, such as the DB
-- instance identifier for a DB instance or the name of a security group.
rsifsSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSourceIdentifier =
    lens _rsifsSourceIdentifier (\s a -> s { _rsifsSourceIdentifier = a })

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSubscriptionName =
    lens _rsifsSubscriptionName (\s a -> s { _rsifsSubscriptionName = a })

instance ToQuery RemoveSourceIdentifierFromSubscription

instance ToPath RemoveSourceIdentifierFromSubscription where
    toPath = const "/"

newtype RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
removeSourceIdentifierFromSubscriptionResponse :: RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription = Nothing
    }

rsifsrEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResponse (Maybe EventSubscription)
rsifsrEventSubscription =
    lens _rsifsrEventSubscription (\s a -> s { _rsifsrEventSubscription = a })

instance FromXML RemoveSourceIdentifierFromSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RemoveSourceIdentifierFromSubscriptionResponse"

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Sv RemoveSourceIdentifierFromSubscription = RDS
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse

    request  = post "RemoveSourceIdentifierFromSubscription"
    response = xmlResponse $ \h x -> RemoveSourceIdentifierFromSubscriptionResponse
        <$> x %| "EventSubscription"
