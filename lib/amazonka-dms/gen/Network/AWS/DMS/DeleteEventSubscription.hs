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
-- Module      : Network.AWS.DMS.DeleteEventSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS DMS event subscription.
--
--
module Network.AWS.DMS.DeleteEventSubscription
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
    , desersEventSubscription
    , desersResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteEventSubscription' smart constructor.
newtype DeleteEventSubscription = DeleteEventSubscription'
  { _desSubscriptionName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desSubscriptionName' - The name of the DMS event notification subscription to be deleted.
deleteEventSubscription
    :: Text -- ^ 'desSubscriptionName'
    -> DeleteEventSubscription
deleteEventSubscription pSubscriptionName_ =
  DeleteEventSubscription' {_desSubscriptionName = pSubscriptionName_}


-- | The name of the DMS event notification subscription to be deleted.
desSubscriptionName :: Lens' DeleteEventSubscription Text
desSubscriptionName = lens _desSubscriptionName (\ s a -> s{_desSubscriptionName = a})

instance AWSRequest DeleteEventSubscription where
        type Rs DeleteEventSubscription =
             DeleteEventSubscriptionResponse
        request = postJSON dms
        response
          = receiveJSON
              (\ s h x ->
                 DeleteEventSubscriptionResponse' <$>
                   (x .?> "EventSubscription") <*> (pure (fromEnum s)))

instance Hashable DeleteEventSubscription where

instance NFData DeleteEventSubscription where

instance ToHeaders DeleteEventSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteEventSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteEventSubscription where
        toJSON DeleteEventSubscription'{..}
          = object
              (catMaybes
                 [Just ("SubscriptionName" .= _desSubscriptionName)])

instance ToPath DeleteEventSubscription where
        toPath = const "/"

instance ToQuery DeleteEventSubscription where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteEventSubscriptionResponse' smart constructor.
data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse'
  { _desersEventSubscription :: !(Maybe EventSubscription)
  , _desersResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desersEventSubscription' - The event subscription that was deleted.
--
-- * 'desersResponseStatus' - -- | The response status code.
deleteEventSubscriptionResponse
    :: Int -- ^ 'desersResponseStatus'
    -> DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse pResponseStatus_ =
  DeleteEventSubscriptionResponse'
    { _desersEventSubscription = Nothing
    , _desersResponseStatus = pResponseStatus_
    }


-- | The event subscription that was deleted.
desersEventSubscription :: Lens' DeleteEventSubscriptionResponse (Maybe EventSubscription)
desersEventSubscription = lens _desersEventSubscription (\ s a -> s{_desersEventSubscription = a})

-- | -- | The response status code.
desersResponseStatus :: Lens' DeleteEventSubscriptionResponse Int
desersResponseStatus = lens _desersResponseStatus (\ s a -> s{_desersResponseStatus = a})

instance NFData DeleteEventSubscriptionResponse where
