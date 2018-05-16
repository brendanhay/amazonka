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
-- Module      : Network.AWS.WorkDocs.DeleteNotificationSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subscription from the specified organization.
--
--
module Network.AWS.WorkDocs.DeleteNotificationSubscription
    (
    -- * Creating a Request
      deleteNotificationSubscription
    , DeleteNotificationSubscription
    -- * Request Lenses
    , dnsSubscriptionId
    , dnsOrganizationId

    -- * Destructuring the Response
    , deleteNotificationSubscriptionResponse
    , DeleteNotificationSubscriptionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteNotificationSubscription' smart constructor.
data DeleteNotificationSubscription = DeleteNotificationSubscription'
  { _dnsSubscriptionId :: !Text
  , _dnsOrganizationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsSubscriptionId' - The ID of the subscription.
--
-- * 'dnsOrganizationId' - The ID of the organization.
deleteNotificationSubscription
    :: Text -- ^ 'dnsSubscriptionId'
    -> Text -- ^ 'dnsOrganizationId'
    -> DeleteNotificationSubscription
deleteNotificationSubscription pSubscriptionId_ pOrganizationId_ =
  DeleteNotificationSubscription'
    { _dnsSubscriptionId = pSubscriptionId_
    , _dnsOrganizationId = pOrganizationId_
    }


-- | The ID of the subscription.
dnsSubscriptionId :: Lens' DeleteNotificationSubscription Text
dnsSubscriptionId = lens _dnsSubscriptionId (\ s a -> s{_dnsSubscriptionId = a})

-- | The ID of the organization.
dnsOrganizationId :: Lens' DeleteNotificationSubscription Text
dnsOrganizationId = lens _dnsOrganizationId (\ s a -> s{_dnsOrganizationId = a})

instance AWSRequest DeleteNotificationSubscription
         where
        type Rs DeleteNotificationSubscription =
             DeleteNotificationSubscriptionResponse
        request = delete workDocs
        response
          = receiveNull DeleteNotificationSubscriptionResponse'

instance Hashable DeleteNotificationSubscription
         where

instance NFData DeleteNotificationSubscription where

instance ToHeaders DeleteNotificationSubscription
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteNotificationSubscription where
        toPath DeleteNotificationSubscription'{..}
          = mconcat
              ["/api/v1/organizations/", toBS _dnsOrganizationId,
               "/subscriptions/", toBS _dnsSubscriptionId]

instance ToQuery DeleteNotificationSubscription where
        toQuery = const mempty

-- | /See:/ 'deleteNotificationSubscriptionResponse' smart constructor.
data DeleteNotificationSubscriptionResponse =
  DeleteNotificationSubscriptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNotificationSubscriptionResponse' with the minimum fields required to make a request.
--
deleteNotificationSubscriptionResponse
    :: DeleteNotificationSubscriptionResponse
deleteNotificationSubscriptionResponse = DeleteNotificationSubscriptionResponse'


instance NFData
           DeleteNotificationSubscriptionResponse
         where
