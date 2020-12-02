{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.UpdateSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of an existing subscription. Only enter values for parameters you want to change. Empty parameters are not updated.
module Network.AWS.Shield.UpdateSubscription
  ( -- * Creating a Request
    updateSubscription,
    UpdateSubscription,

    -- * Request Lenses
    usAutoRenew,

    -- * Destructuring the Response
    updateSubscriptionResponse,
    UpdateSubscriptionResponse,

    -- * Response Lenses
    usrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'updateSubscription' smart constructor.
newtype UpdateSubscription = UpdateSubscription'
  { _usAutoRenew ::
      Maybe AutoRenew
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usAutoRenew' - When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
updateSubscription ::
  UpdateSubscription
updateSubscription = UpdateSubscription' {_usAutoRenew = Nothing}

-- | When you initally create a subscription, @AutoRenew@ is set to @ENABLED@ . If @ENABLED@ , the subscription will be automatically renewed at the end of the existing subscription period. You can change this by submitting an @UpdateSubscription@ request. If the @UpdateSubscription@ request does not included a value for @AutoRenew@ , the existing value for @AutoRenew@ remains unchanged.
usAutoRenew :: Lens' UpdateSubscription (Maybe AutoRenew)
usAutoRenew = lens _usAutoRenew (\s a -> s {_usAutoRenew = a})

instance AWSRequest UpdateSubscription where
  type Rs UpdateSubscription = UpdateSubscriptionResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> UpdateSubscriptionResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateSubscription

instance NFData UpdateSubscription

instance ToHeaders UpdateSubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.UpdateSubscription" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSubscription where
  toJSON UpdateSubscription' {..} =
    object (catMaybes [("AutoRenew" .=) <$> _usAutoRenew])

instance ToPath UpdateSubscription where
  toPath = const "/"

instance ToQuery UpdateSubscription where
  toQuery = const mempty

-- | /See:/ 'updateSubscriptionResponse' smart constructor.
newtype UpdateSubscriptionResponse = UpdateSubscriptionResponse'
  { _usrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateSubscriptionResponse ::
  -- | 'usrsResponseStatus'
  Int ->
  UpdateSubscriptionResponse
updateSubscriptionResponse pResponseStatus_ =
  UpdateSubscriptionResponse'
    { _usrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSubscriptionResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\s a -> s {_usrsResponseStatus = a})

instance NFData UpdateSubscriptionResponse
