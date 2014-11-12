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

-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a source identifier to an existing RDS event notification
-- subscription.
module Network.AWS.RDS.AddSourceIdentifierToSubscription
    (
    -- * Request
      AddSourceIdentifierToSubscriptionMessage
    -- ** Request constructor
    , addSourceIdentifierToSubscription
    -- ** Request lenses
    , asitsmSourceIdentifier
    , asitsmSubscriptionName

    -- * Response
    , AddSourceIdentifierToSubscriptionResult
    -- ** Response constructor
    , addSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , asitsrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data AddSourceIdentifierToSubscriptionMessage = AddSourceIdentifierToSubscriptionMessage
    { _asitsmSourceIdentifier :: Text
    , _asitsmSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddSourceIdentifierToSubscriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsmSourceIdentifier' @::@ 'Text'
--
-- * 'asitsmSubscriptionName' @::@ 'Text'
--
addSourceIdentifierToSubscription :: Text -- ^ 'asitsmSubscriptionName'
                                  -> Text -- ^ 'asitsmSourceIdentifier'
                                  -> AddSourceIdentifierToSubscriptionMessage
addSourceIdentifierToSubscription p1 p2 = AddSourceIdentifierToSubscriptionMessage
    { _asitsmSubscriptionName = p1
    , _asitsmSourceIdentifier = p2
    }

-- | The identifier of the event source to be added. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it cannot end with a hyphen or contain two consecutive hyphens.
-- Constraints: If the source type is a DB instance, then a
-- DBInstanceIdentifier must be supplied. If the source type is a DB
-- security group, a DBSecurityGroupName must be supplied. If the source
-- type is a DB parameter group, a DBParameterGroupName must be supplied. If
-- the source type is a DB snapshot, a DBSnapshotIdentifier must be
-- supplied.
asitsmSourceIdentifier :: Lens' AddSourceIdentifierToSubscriptionMessage Text
asitsmSourceIdentifier =
    lens _asitsmSourceIdentifier (\s a -> s { _asitsmSourceIdentifier = a })

-- | The name of the RDS event notification subscription you want to add a
-- source identifier to.
asitsmSubscriptionName :: Lens' AddSourceIdentifierToSubscriptionMessage Text
asitsmSubscriptionName =
    lens _asitsmSubscriptionName (\s a -> s { _asitsmSubscriptionName = a })

instance ToQuery AddSourceIdentifierToSubscriptionMessage

instance ToPath AddSourceIdentifierToSubscriptionMessage where
    toPath = const "/"

newtype AddSourceIdentifierToSubscriptionResult = AddSourceIdentifierToSubscriptionResult
    { _asitsrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'AddSourceIdentifierToSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
addSourceIdentifierToSubscriptionResponse :: AddSourceIdentifierToSubscriptionResult
addSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResult
    { _asitsrEventSubscription = Nothing
    }

asitsrEventSubscription :: Lens' AddSourceIdentifierToSubscriptionResult (Maybe EventSubscription)
asitsrEventSubscription =
    lens _asitsrEventSubscription (\s a -> s { _asitsrEventSubscription = a })

instance FromXML AddSourceIdentifierToSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AddSourceIdentifierToSubscriptionResult"

instance AWSRequest AddSourceIdentifierToSubscriptionMessage where
    type Sv AddSourceIdentifierToSubscriptionMessage = RDS
    type Rs AddSourceIdentifierToSubscriptionMessage = AddSourceIdentifierToSubscriptionResult

    request  = post "AddSourceIdentifierToSubscription"
    response = xmlResponse $ \h x -> AddSourceIdentifierToSubscriptionResult
        <$> x %| "EventSubscription"
