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
-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an RDS event notification subscription.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteEventSubscription.html AWS API Reference> for DeleteEventSubscription.
module Network.AWS.RDS.DeleteEventSubscription
    (
    -- * Creating a Request
      deleteEventSubscription
    , DeleteEventSubscription
    -- * Request Lenses
    , desSubscriptionName

    -- * Destructuring the Response
    , deleteEventSubscriptionResponse
    , DeleteEventSubscriptionResponse
    -- * Response Lenses
    , drsEventSubscription
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
    { _desSubscriptionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSubscriptionName'
deleteEventSubscription
    :: Text -- ^ 'desSubscriptionName'
    -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName_ =
    DeleteEventSubscription'
    { _desSubscriptionName = pSubscriptionName_
    }

-- | The name of the RDS event notification subscription you want to delete.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a});

instance AWSRequest DeleteEventSubscription where
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "DeleteEventSubscriptionResult"
              (\ s h x ->
                 DeleteEventSubscriptionResponse' <$>
                   (x .@? "EventSubscription") <*> (pure (fromEnum s)))

instance ToHeaders DeleteEventSubscription where
        toHeaders = const mempty

instance ToPath DeleteEventSubscription where
        toPath = const "/"

instance ToQuery DeleteEventSubscription where
        toQuery DeleteEventSubscription'{..}
          = mconcat
              ["Action" =:
                 ("DeleteEventSubscription" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "SubscriptionName" =: _desSubscriptionName]

-- | /See:/ 'deleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
    { _drsEventSubscription :: !(Maybe EventSubscription)
    , _drsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsEventSubscription'
--
-- * 'drsStatus'
deleteEventSubscriptionResponse
    :: Int -- ^ 'drsStatus'
    -> DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pStatus_ =
    DeleteEventSubscriptionResponse'
    { _drsEventSubscription = Nothing
    , _drsStatus = pStatus_
    }

-- | Undocumented member.
drsEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
drsEventSubscription = lens _drsEventSubscription (\ s a -> s{_drsEventSubscription = a});

-- | The response status code.
drsStatus :: Lens' DeleteEventSubscriptionResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
