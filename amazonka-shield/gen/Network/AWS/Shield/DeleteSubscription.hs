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
-- Module      : Network.AWS.Shield.DeleteSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes AWS Shield Advanced from an account. AWS Shield Advanced requires a 1-year subscription commitment. You cannot delete a subscription prior to the completion of that commitment.
--
--
module Network.AWS.Shield.DeleteSubscription
    (
    -- * Creating a Request
      deleteSubscription
    , DeleteSubscription

    -- * Destructuring the Response
    , deleteSubscriptionResponse
    , DeleteSubscriptionResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'deleteSubscription' smart constructor.
data DeleteSubscription =
  DeleteSubscription'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscription' with the minimum fields required to make a request.
--
deleteSubscription
    :: DeleteSubscription
deleteSubscription = DeleteSubscription'


instance AWSRequest DeleteSubscription where
        type Rs DeleteSubscription =
             DeleteSubscriptionResponse
        request = postJSON shield
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSubscriptionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteSubscription where

instance NFData DeleteSubscription where

instance ToHeaders DeleteSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.DeleteSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubscription where
        toJSON = const (Object mempty)

instance ToPath DeleteSubscription where
        toPath = const "/"

instance ToQuery DeleteSubscription where
        toQuery = const mempty

-- | /See:/ 'deleteSubscriptionResponse' smart constructor.
newtype DeleteSubscriptionResponse = DeleteSubscriptionResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteSubscriptionResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteSubscriptionResponse
deleteSubscriptionResponse pResponseStatus_ =
  DeleteSubscriptionResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteSubscriptionResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteSubscriptionResponse where
