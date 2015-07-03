{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes a source identifier from an existing RDS event notification
-- subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RemoveSourceIdentifierFromSubscription.html>
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
    , rsifsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'removeSourceIdentifierFromSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsSubscriptionName'
--
-- * 'rsifsSourceIdentifier'
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription'
    { _rsifsSubscriptionName :: !Text
    , _rsifsSourceIdentifier :: !Text
    } deriving (Eq,Read,Show)

-- | 'RemoveSourceIdentifierFromSubscription' smart constructor.
removeSourceIdentifierFromSubscription :: Text -> Text -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription pSubscriptionName pSourceIdentifier =
    RemoveSourceIdentifierFromSubscription'
    { _rsifsSubscriptionName = pSubscriptionName
    , _rsifsSourceIdentifier = pSourceIdentifier
    }

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSubscriptionName = lens _rsifsSubscriptionName (\ s a -> s{_rsifsSubscriptionName = a});

-- | The source identifier to be removed from the subscription, such as the
-- __DB instance identifier__ for a DB instance or the name of a security
-- group.
rsifsSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSourceIdentifier = lens _rsifsSourceIdentifier (\ s a -> s{_rsifsSourceIdentifier = a});

instance AWSRequest
         RemoveSourceIdentifierFromSubscription where
        type Sv RemoveSourceIdentifierFromSubscription = RDS
        type Rs RemoveSourceIdentifierFromSubscription =
             RemoveSourceIdentifierFromSubscriptionResponse
        request = post
        response
          = receiveXMLWrapper
              "RemoveSourceIdentifierFromSubscriptionResult"
              (\ s h x ->
                 RemoveSourceIdentifierFromSubscriptionResponse' <$>
                   (x .@? "EventSubscription") <*> (pure (fromEnum s)))

instance ToHeaders
         RemoveSourceIdentifierFromSubscription where
        toHeaders = const mempty

instance ToPath
         RemoveSourceIdentifierFromSubscription where
        toPath = const "/"

instance ToQuery
         RemoveSourceIdentifierFromSubscription where
        toQuery RemoveSourceIdentifierFromSubscription'{..}
          = mconcat
              ["Action" =:
                 ("RemoveSourceIdentifierFromSubscription" ::
                    ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SubscriptionName" =: _rsifsSubscriptionName,
               "SourceIdentifier" =: _rsifsSourceIdentifier]

-- | /See:/ 'removeSourceIdentifierFromSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsrEventSubscription'
--
-- * 'rsifsrStatus'
data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse'
    { _rsifsrEventSubscription :: !(Maybe EventSubscription)
    , _rsifsrStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'RemoveSourceIdentifierFromSubscriptionResponse' smart constructor.
removeSourceIdentifierFromSubscriptionResponse :: Int -> RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscriptionResponse pStatus =
    RemoveSourceIdentifierFromSubscriptionResponse'
    { _rsifsrEventSubscription = Nothing
    , _rsifsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
rsifsrEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResponse (Maybe EventSubscription)
rsifsrEventSubscription = lens _rsifsrEventSubscription (\ s a -> s{_rsifsrEventSubscription = a});

-- | FIXME: Undocumented member.
rsifsrStatus :: Lens' RemoveSourceIdentifierFromSubscriptionResponse Int
rsifsrStatus = lens _rsifsrStatus (\ s a -> s{_rsifsrStatus = a});
