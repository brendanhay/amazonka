{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , asitsrqSubscriptionName
    , asitsrqSourceIdentifier

    -- * Response
    , AddSourceIdentifierToSubscriptionResponse
    -- ** Response constructor
    , addSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , asitsrsEventSubscription
    , asitsrsStatus
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
-- * 'asitsrqSubscriptionName'
--
-- * 'asitsrqSourceIdentifier'
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
    { _asitsrqSubscriptionName :: !Text
    , _asitsrqSourceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddSourceIdentifierToSubscription' smart constructor.
addSourceIdentifierToSubscription :: Text -> Text -> AddSourceIdentifierToSubscription
addSourceIdentifierToSubscription pSubscriptionName_ pSourceIdentifier_ =
    AddSourceIdentifierToSubscription'
    { _asitsrqSubscriptionName = pSubscriptionName_
    , _asitsrqSourceIdentifier = pSourceIdentifier_
    }

-- | The name of the RDS event notification subscription you want to add a
-- source identifier to.
asitsrqSubscriptionName :: Lens' AddSourceIdentifierToSubscription Text
asitsrqSubscriptionName = lens _asitsrqSubscriptionName (\ s a -> s{_asitsrqSubscriptionName = a});

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
asitsrqSourceIdentifier :: Lens' AddSourceIdentifierToSubscription Text
asitsrqSourceIdentifier = lens _asitsrqSourceIdentifier (\ s a -> s{_asitsrqSourceIdentifier = a});

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
               "SubscriptionName" =: _asitsrqSubscriptionName,
               "SourceIdentifier" =: _asitsrqSourceIdentifier]

-- | /See:/ 'addSourceIdentifierToSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsrsEventSubscription'
--
-- * 'asitsrsStatus'
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
    { _asitsrsEventSubscription :: !(Maybe EventSubscription)
    , _asitsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddSourceIdentifierToSubscriptionResponse' smart constructor.
addSourceIdentifierToSubscriptionResponse :: Int -> AddSourceIdentifierToSubscriptionResponse
addSourceIdentifierToSubscriptionResponse pStatus_ =
    AddSourceIdentifierToSubscriptionResponse'
    { _asitsrsEventSubscription = Nothing
    , _asitsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
asitsrsEventSubscription :: Lens' AddSourceIdentifierToSubscriptionResponse (Maybe EventSubscription)
asitsrsEventSubscription = lens _asitsrsEventSubscription (\ s a -> s{_asitsrsEventSubscription = a});

-- | FIXME: Undocumented member.
asitsrsStatus :: Lens' AddSourceIdentifierToSubscriptionResponse Int
asitsrsStatus = lens _asitsrsStatus (\ s a -> s{_asitsrsStatus = a});
