{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a source identifier to an existing RDS event notification
-- subscription.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AddSourceIdentifierToSubscription.html AWS API Reference> for AddSourceIdentifierToSubscription.
module Network.AWS.RDS.AddSourceIdentifierToSubscription
    (
    -- * Creating a Request
      addSourceIdentifierToSubscription
    , AddSourceIdentifierToSubscription
    -- * Request Lenses
    , asitsSubscriptionName
    , asitsSourceIdentifier

    -- * Destructuring the Response
    , addSourceIdentifierToSubscriptionResponse
    , AddSourceIdentifierToSubscriptionResponse
    -- * Response Lenses
    , asitsrsEventSubscription
    , asitsrsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'addSourceIdentifierToSubscription' smart constructor.
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
    { _asitsSubscriptionName :: !Text
    , _asitsSourceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddSourceIdentifierToSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asitsSubscriptionName'
--
-- * 'asitsSourceIdentifier'
addSourceIdentifierToSubscription
    :: Text -- ^ 'asitsSubscriptionName'
    -> Text -- ^ 'asitsSourceIdentifier'
    -> AddSourceIdentifierToSubscription
addSourceIdentifierToSubscription pSubscriptionName_ pSourceIdentifier_ =
    AddSourceIdentifierToSubscription'
    { _asitsSubscriptionName = pSubscriptionName_
    , _asitsSourceIdentifier = pSourceIdentifier_
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
-- -   If the source type is a DB instance, then a 'DBInstanceIdentifier'
--     must be supplied.
-- -   If the source type is a DB security group, a 'DBSecurityGroupName'
--     must be supplied.
-- -   If the source type is a DB parameter group, a 'DBParameterGroupName'
--     must be supplied.
-- -   If the source type is a DB snapshot, a 'DBSnapshotIdentifier' must
--     be supplied.
asitsSourceIdentifier :: Lens' AddSourceIdentifierToSubscription Text
asitsSourceIdentifier = lens _asitsSourceIdentifier (\ s a -> s{_asitsSourceIdentifier = a});

instance AWSRequest AddSourceIdentifierToSubscription
         where
        type Rs AddSourceIdentifierToSubscription =
             AddSourceIdentifierToSubscriptionResponse
        request = postQuery rDS
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
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
    { _asitsrsEventSubscription :: !(Maybe EventSubscription)
    , _asitsrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddSourceIdentifierToSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asitsrsEventSubscription'
--
-- * 'asitsrsResponseStatus'
addSourceIdentifierToSubscriptionResponse
    :: Int -- ^ 'asitsrsResponseStatus'
    -> AddSourceIdentifierToSubscriptionResponse
addSourceIdentifierToSubscriptionResponse pResponseStatus_ =
    AddSourceIdentifierToSubscriptionResponse'
    { _asitsrsEventSubscription = Nothing
    , _asitsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
asitsrsEventSubscription :: Lens' AddSourceIdentifierToSubscriptionResponse (Maybe EventSubscription)
asitsrsEventSubscription = lens _asitsrsEventSubscription (\ s a -> s{_asitsrsEventSubscription = a});

-- | The response status code.
asitsrsResponseStatus :: Lens' AddSourceIdentifierToSubscriptionResponse Int
asitsrsResponseStatus = lens _asitsrsResponseStatus (\ s a -> s{_asitsrsResponseStatus = a});
