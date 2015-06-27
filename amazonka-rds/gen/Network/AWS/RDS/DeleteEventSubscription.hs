{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes an RDS event notification subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteEventSubscription.html>
module Network.AWS.RDS.DeleteEventSubscription
    (
    -- * Request
      DeleteEventSubscription
    -- ** Request constructor
    , deleteEventSubscription
    -- ** Request lenses
    , desSubscriptionName

    -- * Response
    , DeleteEventSubscriptionResponse
    -- ** Response constructor
    , deleteEventSubscriptionResponse
    -- ** Response lenses
    , delEventSubscription
    , delStatus
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
    } deriving (Eq,Read,Show)

-- | 'DeleteEventSubscription' smart constructor.
deleteEventSubscription :: Text -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName =
    DeleteEventSubscription'
    { _desSubscriptionName = pSubscriptionName
    }

-- | The name of the RDS event notification subscription you want to delete.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a});

instance AWSRequest DeleteEventSubscription where
        type Sv DeleteEventSubscription = RDS
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = post
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
-- * 'delEventSubscription'
--
-- * 'delStatus'
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
    { _delEventSubscription :: !(Maybe EventSubscription)
    , _delStatus            :: !Int
    } deriving (Eq,Read,Show)

-- | 'DeleteEventSubscriptionResponse' smart constructor.
deleteEventSubscriptionResponse :: Int -> DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pStatus =
    DeleteEventSubscriptionResponse'
    { _delEventSubscription = Nothing
    , _delStatus = pStatus
    }

-- | FIXME: Undocumented member.
delEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
delEventSubscription = lens _delEventSubscription (\ s a -> s{_delEventSubscription = a});

-- | FIXME: Undocumented member.
delStatus :: Lens' DeleteEventSubscriptionResponse Int
delStatus = lens _delStatus (\ s a -> s{_delStatus = a});
