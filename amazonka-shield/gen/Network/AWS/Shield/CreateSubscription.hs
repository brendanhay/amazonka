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
-- Module      : Network.AWS.Shield.CreateSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates AWS Shield Advanced for an account.
--
--
module Network.AWS.Shield.CreateSubscription
    (
    -- * Creating a Request
      createSubscription
    , CreateSubscription

    -- * Destructuring the Response
    , createSubscriptionResponse
    , CreateSubscriptionResponse
    -- * Response Lenses
    , csrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types
import Network.AWS.Shield.Types.Product

-- | /See:/ 'createSubscription' smart constructor.
data CreateSubscription =
  CreateSubscription'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscription' with the minimum fields required to make a request.
--
createSubscription
    :: CreateSubscription
createSubscription = CreateSubscription'


instance AWSRequest CreateSubscription where
        type Rs CreateSubscription =
             CreateSubscriptionResponse
        request = postJSON shield
        response
          = receiveEmpty
              (\ s h x ->
                 CreateSubscriptionResponse' <$> (pure (fromEnum s)))

instance Hashable CreateSubscription where

instance NFData CreateSubscription where

instance ToHeaders CreateSubscription where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSShield_20160616.CreateSubscription" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSubscription where
        toJSON = const (Object mempty)

instance ToPath CreateSubscription where
        toPath = const "/"

instance ToQuery CreateSubscription where
        toQuery = const mempty

-- | /See:/ 'createSubscriptionResponse' smart constructor.
newtype CreateSubscriptionResponse = CreateSubscriptionResponse'
  { _csrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsResponseStatus' - -- | The response status code.
createSubscriptionResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateSubscriptionResponse
createSubscriptionResponse pResponseStatus_ =
  CreateSubscriptionResponse' {_csrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSubscriptionResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateSubscriptionResponse where
