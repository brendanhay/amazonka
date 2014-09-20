{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=RemoveSourceIdentifierFromSubscription
-- &SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T012415Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 6f0b82bf-68e9-11e2-b97b-43c6362ec60d.
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
    (
    -- * Request
      RemoveSourceIdentifierFromSubscription
    -- ** Request constructor
    , removeSourceIdentifierFromSubscription
    -- ** Request lenses
    , rsifsSubscriptionName
    , rsifsSourceIdentifier

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResponse
    -- ** Response constructor
    , removeSourceIdentifierFromSubscriptionResponse
    -- ** Response lenses
    , rsifsrEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { _rsifsSubscriptionName :: Text
    , _rsifsSourceIdentifier :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveSourceIdentifierFromSubscription' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionName ::@ @Text@
--
-- * @SourceIdentifier ::@ @Text@
--
removeSourceIdentifierFromSubscription :: Text -- ^ 'rsifsSubscriptionName'
                                       -> Text -- ^ 'rsifsSourceIdentifier'
                                       -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription p1 p2 = RemoveSourceIdentifierFromSubscription
    { _rsifsSubscriptionName = p1
    , _rsifsSourceIdentifier = p2
    }

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSubscriptionName =
    lens _rsifsSubscriptionName (\s a -> s { _rsifsSubscriptionName = a })

-- | The source identifier to be removed from the subscription, such as the DB
-- instance identifier for a DB instance or the name of a security group.
rsifsSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSourceIdentifier =
    lens _rsifsSourceIdentifier (\s a -> s { _rsifsSourceIdentifier = a })

instance ToQuery RemoveSourceIdentifierFromSubscription where
    toQuery = genericQuery def

newtype RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription :: Maybe EventSubscription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RemoveSourceIdentifierFromSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @EventSubscription ::@ @Maybe EventSubscription@
--
removeSourceIdentifierFromSubscriptionResponse :: RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription = Nothing
    }

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
rsifsrEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResponse (Maybe EventSubscription)
rsifsrEventSubscription =
    lens _rsifsrEventSubscription
         (\s a -> s { _rsifsrEventSubscription = a })

instance FromXML RemoveSourceIdentifierFromSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Sv RemoveSourceIdentifierFromSubscription = RDS
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse

    request = post "RemoveSourceIdentifierFromSubscription"
    response _ = xmlResponse
