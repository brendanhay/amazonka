{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
      DeleteEventSubscription
    , deleteEventSubscription
    -- * Request Lenses
    , desSubscriptionName

    -- * Destructuring the Response
    , DeleteEventSubscriptionResponse
    , deleteEventSubscriptionResponse
    -- * Response Lenses
    , drsEventSubscription
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteEventSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desSubscriptionName'
newtype DeleteEventSubscription = DeleteEventSubscription'
    { _desSubscriptionName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEventSubscription' smart constructor.
deleteEventSubscription :: Text -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName_ =
    DeleteEventSubscription'
    { _desSubscriptionName = pSubscriptionName_
    }

-- | The name of the RDS event notification subscription you want to delete.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a});

instance AWSRequest DeleteEventSubscription where
        type Sv DeleteEventSubscription = RDS
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsEventSubscription'
--
-- * 'drsStatus'
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
    { _drsEventSubscription :: !(Maybe EventSubscription)
    , _drsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEventSubscriptionResponse' smart constructor.
deleteEventSubscriptionResponse :: Int -> DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pStatus_ =
    DeleteEventSubscriptionResponse'
    { _drsEventSubscription = Nothing
    , _drsStatus = pStatus_
    }

-- | Undocumented member.
drsEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
drsEventSubscription = lens _drsEventSubscription (\ s a -> s{_drsEventSubscription = a});

-- | Undocumented member.
drsStatus :: Lens' DeleteEventSubscriptionResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
