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
-- Module      : Network.AWS.Shield.GetSubscriptionState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @SubscriptionState@ , either @Active@ or @Inactive@ .
--
--
module Network.AWS.Shield.GetSubscriptionState
    (
    -- * Creating a Request
      getSubscriptionState
    , GetSubscriptionState

    -- * Destructuring the Response
    , getSubscriptionStateResponse
    , GetSubscriptionStateResponse
    -- * Response Lenses
    , gssrsResponseStatus
    , gssrsSubscriptionState
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'getSubscriptionState' smart constructor.
data GetSubscriptionState =
  GetSubscriptionState'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSubscriptionState' with the minimum fields required to make a request.
--
getSubscriptionState
    :: GetSubscriptionState
getSubscriptionState = GetSubscriptionState'


instance AWSRequest GetSubscriptionState where
        type Rs GetSubscriptionState =
             GetSubscriptionStateResponse
        request = postJSON shield
        response
          = receiveJSON
              (\ s h x ->
                 GetSubscriptionStateResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "SubscriptionState"))

instance Hashable GetSubscriptionState where

instance NFData GetSubscriptionState where

instance ToHeaders GetSubscriptionState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.GetSubscriptionState" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSubscriptionState where
        toJSON = const (Object mempty)

instance ToPath GetSubscriptionState where
        toPath = const "/"

instance ToQuery GetSubscriptionState where
        toQuery = const mempty

-- | /See:/ 'getSubscriptionStateResponse' smart constructor.
data GetSubscriptionStateResponse = GetSubscriptionStateResponse'
  { _gssrsResponseStatus    :: !Int
  , _gssrsSubscriptionState :: !SubscriptionState
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSubscriptionStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsResponseStatus' - -- | The response status code.
--
-- * 'gssrsSubscriptionState' - The status of the subscription.
getSubscriptionStateResponse
    :: Int -- ^ 'gssrsResponseStatus'
    -> SubscriptionState -- ^ 'gssrsSubscriptionState'
    -> GetSubscriptionStateResponse
getSubscriptionStateResponse pResponseStatus_ pSubscriptionState_ =
  GetSubscriptionStateResponse'
    { _gssrsResponseStatus = pResponseStatus_
    , _gssrsSubscriptionState = pSubscriptionState_
    }


-- | -- | The response status code.
gssrsResponseStatus :: Lens' GetSubscriptionStateResponse Int
gssrsResponseStatus = lens _gssrsResponseStatus (\ s a -> s{_gssrsResponseStatus = a})

-- | The status of the subscription.
gssrsSubscriptionState :: Lens' GetSubscriptionStateResponse SubscriptionState
gssrsSubscriptionState = lens _gssrsSubscriptionState (\ s a -> s{_gssrsSubscriptionState = a})

instance NFData GetSubscriptionStateResponse where
