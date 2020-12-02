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
-- Module      : Network.AWS.CostExplorer.CreateAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a subscription to a cost anomaly detection monitor. You can use each subscription to define subscribers with email or SNS notifications. Email subscribers can set a dollar threshold and a time frequency for receiving notifications.
module Network.AWS.CostExplorer.CreateAnomalySubscription
  ( -- * Creating a Request
    createAnomalySubscription,
    CreateAnomalySubscription,

    -- * Request Lenses
    casAnomalySubscription,

    -- * Destructuring the Response
    createAnomalySubscriptionResponse,
    CreateAnomalySubscriptionResponse,

    -- * Response Lenses
    casrsResponseStatus,
    casrsSubscriptionARN,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAnomalySubscription' smart constructor.
newtype CreateAnomalySubscription = CreateAnomalySubscription'
  { _casAnomalySubscription ::
      AnomalySubscription
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAnomalySubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casAnomalySubscription' - The cost anomaly subscription object that you want to create.
createAnomalySubscription ::
  -- | 'casAnomalySubscription'
  AnomalySubscription ->
  CreateAnomalySubscription
createAnomalySubscription pAnomalySubscription_ =
  CreateAnomalySubscription'
    { _casAnomalySubscription =
        pAnomalySubscription_
    }

-- | The cost anomaly subscription object that you want to create.
casAnomalySubscription :: Lens' CreateAnomalySubscription AnomalySubscription
casAnomalySubscription = lens _casAnomalySubscription (\s a -> s {_casAnomalySubscription = a})

instance AWSRequest CreateAnomalySubscription where
  type
    Rs CreateAnomalySubscription =
      CreateAnomalySubscriptionResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          CreateAnomalySubscriptionResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "SubscriptionArn")
      )

instance Hashable CreateAnomalySubscription

instance NFData CreateAnomalySubscription

instance ToHeaders CreateAnomalySubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.CreateAnomalySubscription" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateAnomalySubscription where
  toJSON CreateAnomalySubscription' {..} =
    object
      ( catMaybes
          [Just ("AnomalySubscription" .= _casAnomalySubscription)]
      )

instance ToPath CreateAnomalySubscription where
  toPath = const "/"

instance ToQuery CreateAnomalySubscription where
  toQuery = const mempty

-- | /See:/ 'createAnomalySubscriptionResponse' smart constructor.
data CreateAnomalySubscriptionResponse = CreateAnomalySubscriptionResponse'
  { _casrsResponseStatus ::
      !Int,
    _casrsSubscriptionARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casrsResponseStatus' - -- | The response status code.
--
-- * 'casrsSubscriptionARN' - The unique identifier of your newly created cost anomaly subscription.
createAnomalySubscriptionResponse ::
  -- | 'casrsResponseStatus'
  Int ->
  -- | 'casrsSubscriptionARN'
  Text ->
  CreateAnomalySubscriptionResponse
createAnomalySubscriptionResponse
  pResponseStatus_
  pSubscriptionARN_ =
    CreateAnomalySubscriptionResponse'
      { _casrsResponseStatus =
          pResponseStatus_,
        _casrsSubscriptionARN = pSubscriptionARN_
      }

-- | -- | The response status code.
casrsResponseStatus :: Lens' CreateAnomalySubscriptionResponse Int
casrsResponseStatus = lens _casrsResponseStatus (\s a -> s {_casrsResponseStatus = a})

-- | The unique identifier of your newly created cost anomaly subscription.
casrsSubscriptionARN :: Lens' CreateAnomalySubscriptionResponse Text
casrsSubscriptionARN = lens _casrsSubscriptionARN (\s a -> s {_casrsSubscriptionARN = a})

instance NFData CreateAnomalySubscriptionResponse
