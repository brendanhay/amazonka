{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Adds a source identifier to an existing RDS event notification
-- subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AddSourceIdentifierToSubscription.html>
module Network.AWS.RDS.AddSourceIdentifierToSubscription
    (
    -- * Request
      AddSourceIdentifierToSubscription
    -- ** Request constructor
    , addSourceIdentifierToSubscription
    -- ** Request lenses
    , asitsSubscriptionName
    , asitsSourceIdentifier

    -- * Response
    , AddSourceIdentifierToSubscriptionResponse
    -- ** Response constructor
    , addSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , asitsrEventSubscription
    , asitsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'addSourceIdentifierToSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsSubscriptionName'
--
-- * 'asitsSourceIdentifier'
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
    { _asitsSubscriptionName :: !Text
    , _asitsSourceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddSourceIdentifierToSubscription' smart constructor.
addSourceIdentifierToSubscription :: Text -> Text -> AddSourceIdentifierToSubscription
addSourceIdentifierToSubscription pSubscriptionName pSourceIdentifier =
    AddSourceIdentifierToSubscription'
    { _asitsSubscriptionName = pSubscriptionName
    , _asitsSourceIdentifier = pSourceIdentifier
    }

-- | The name of the RDS event notification subscription you want to add a
-- source identifier to.
asitsSubscriptionName :: Lens' AddSourceIdentifierToSubscription Text
asitsSubscriptionName = lens _asitsSubscriptionName (\ s a -> s{_asitsSubscriptionName = a});

-- | The identifier of the event source to be added. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it cannot end with a hyphen or contain two consecutive hyphens.
--
-- Constraints:
--
-- -   If the source type is a DB instance, then a @DBInstanceIdentifier@
--     must be supplied.
-- -   If the source type is a DB security group, a @DBSecurityGroupName@
--     must be supplied.
-- -   If the source type is a DB parameter group, a @DBParameterGroupName@
--     must be supplied.
-- -   If the source type is a DB snapshot, a @DBSnapshotIdentifier@ must
--     be supplied.
asitsSourceIdentifier :: Lens' AddSourceIdentifierToSubscription Text
asitsSourceIdentifier = lens _asitsSourceIdentifier (\ s a -> s{_asitsSourceIdentifier = a});

instance AWSRequest AddSourceIdentifierToSubscription
         where
        type Sv AddSourceIdentifierToSubscription = RDS
        type Rs AddSourceIdentifierToSubscription =
             AddSourceIdentifierToSubscriptionResponse
        request = post
        response
          = receiveXMLWrapper
              "AddSourceIdentifierToSubscriptionResult"
              (\ s h x ->
                 AddSourceIdentifierToSubscriptionResponse' <$>
                   (x .@? "EventSubscription") <*> (pure (fromEnum s)))

instance ToHeaders AddSourceIdentifierToSubscription
         where
        toHeaders = const mempty

instance ToPath AddSourceIdentifierToSubscription
         where
        toPath = const "/"

instance ToQuery AddSourceIdentifierToSubscription
         where
        toQuery AddSourceIdentifierToSubscription'{..}
          = mconcat
              ["Action" =:
                 ("AddSourceIdentifierToSubscription" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SubscriptionName" =: _asitsSubscriptionName,
               "SourceIdentifier" =: _asitsSourceIdentifier]

-- | /See:/ 'addSourceIdentifierToSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsrEventSubscription'
--
-- * 'asitsrStatus'
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
    { _asitsrEventSubscription :: !(Maybe EventSubscription)
    , _asitsrStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddSourceIdentifierToSubscriptionResponse' smart constructor.
addSourceIdentifierToSubscriptionResponse :: Int -> AddSourceIdentifierToSubscriptionResponse
addSourceIdentifierToSubscriptionResponse pStatus =
    AddSourceIdentifierToSubscriptionResponse'
    { _asitsrEventSubscription = Nothing
    , _asitsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
asitsrEventSubscription :: Lens' AddSourceIdentifierToSubscriptionResponse (Maybe EventSubscription)
asitsrEventSubscription = lens _asitsrEventSubscription (\ s a -> s{_asitsrEventSubscription = a});

-- | FIXME: Undocumented member.
asitsrStatus :: Lens' AddSourceIdentifierToSubscriptionResponse Int
asitsrStatus = lens _asitsrStatus (\ s a -> s{_asitsrStatus = a});
