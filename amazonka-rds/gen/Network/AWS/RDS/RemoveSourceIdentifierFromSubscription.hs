{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes a source identifier from an existing RDS event notification
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
    , rsifsrqSubscriptionName
    , rsifsrqSourceIdentifier

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResponse
    -- ** Response constructor
    , removeSourceIdentifierFromSubscriptionResponse
    -- ** Response lenses
    , rsifsrsEventSubscription
    , rsifsrsStatus
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
-- * 'rsifsrqSubscriptionName'
--
-- * 'rsifsrqSourceIdentifier'
data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription'
    { _rsifsrqSubscriptionName :: !Text
    , _rsifsrqSourceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveSourceIdentifierFromSubscription' smart constructor.
removeSourceIdentifierFromSubscription :: Text -> Text -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription pSubscriptionName pSourceIdentifier =
    RemoveSourceIdentifierFromSubscription'
    { _rsifsrqSubscriptionName = pSubscriptionName
    , _rsifsrqSourceIdentifier = pSourceIdentifier
    }

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsrqSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsrqSubscriptionName = lens _rsifsrqSubscriptionName (\ s a -> s{_rsifsrqSubscriptionName = a});

-- | The source identifier to be removed from the subscription, such as the
-- __DB instance identifier__ for a DB instance or the name of a security
-- group.
rsifsrqSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsrqSourceIdentifier = lens _rsifsrqSourceIdentifier (\ s a -> s{_rsifsrqSourceIdentifier = a});

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
               "SubscriptionName" =: _rsifsrqSubscriptionName,
               "SourceIdentifier" =: _rsifsrqSourceIdentifier]

-- | /See:/ 'removeSourceIdentifierFromSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsrsEventSubscription'
--
-- * 'rsifsrsStatus'
data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse'
    { _rsifsrsEventSubscription :: !(Maybe EventSubscription)
    , _rsifsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveSourceIdentifierFromSubscriptionResponse' smart constructor.
removeSourceIdentifierFromSubscriptionResponse :: Int -> RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscriptionResponse pStatus =
    RemoveSourceIdentifierFromSubscriptionResponse'
    { _rsifsrsEventSubscription = Nothing
    , _rsifsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
rsifsrsEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResponse (Maybe EventSubscription)
rsifsrsEventSubscription = lens _rsifsrsEventSubscription (\ s a -> s{_rsifsrsEventSubscription = a});

-- | FIXME: Undocumented member.
rsifsrsStatus :: Lens' RemoveSourceIdentifierFromSubscriptionResponse Int
rsifsrsStatus = lens _rsifsrsStatus (\ s a -> s{_rsifsrsStatus = a});
