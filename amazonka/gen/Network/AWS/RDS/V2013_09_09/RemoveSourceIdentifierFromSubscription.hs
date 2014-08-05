{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription
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
module Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { _rsifsmSubscriptionName :: Text
      -- ^ The name of the RDS event notification subscription you want to
      -- remove a source identifier from.
    , _rsifsmSourceIdentifier :: Text
      -- ^ The source identifier to be removed from the subscription, such
      -- as the DB instance identifier for a DB instance or the name of a
      -- security group.
    } deriving (Show, Generic)

makeLenses ''RemoveSourceIdentifierFromSubscription

instance ToQuery RemoveSourceIdentifierFromSubscription where
    toQuery = genericToQuery def

data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _esyEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Show, Generic)

makeLenses ''RemoveSourceIdentifierFromSubscriptionResponse

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Sv RemoveSourceIdentifierFromSubscription = RDS
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse

    request = post "RemoveSourceIdentifierFromSubscription"
    response _ = cursorResponse $ \hs xml ->
        pure RemoveSourceIdentifierFromSubscriptionResponse
            <*> xml %|? "EventSubscription"
