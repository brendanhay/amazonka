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
-- Module      : Network.AWS.CostExplorer.DeleteAnomalySubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly subscription.
module Network.AWS.CostExplorer.DeleteAnomalySubscription
  ( -- * Creating a Request
    deleteAnomalySubscription,
    DeleteAnomalySubscription,

    -- * Request Lenses
    dasSubscriptionARN,

    -- * Destructuring the Response
    deleteAnomalySubscriptionResponse,
    DeleteAnomalySubscriptionResponse,

    -- * Response Lenses
    dasrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAnomalySubscription' smart constructor.
newtype DeleteAnomalySubscription = DeleteAnomalySubscription'
  { _dasSubscriptionARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAnomalySubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasSubscriptionARN' - The unique identifier of the cost anomaly subscription that you want to delete.
deleteAnomalySubscription ::
  -- | 'dasSubscriptionARN'
  Text ->
  DeleteAnomalySubscription
deleteAnomalySubscription pSubscriptionARN_ =
  DeleteAnomalySubscription'
    { _dasSubscriptionARN =
        pSubscriptionARN_
    }

-- | The unique identifier of the cost anomaly subscription that you want to delete.
dasSubscriptionARN :: Lens' DeleteAnomalySubscription Text
dasSubscriptionARN = lens _dasSubscriptionARN (\s a -> s {_dasSubscriptionARN = a})

instance AWSRequest DeleteAnomalySubscription where
  type
    Rs DeleteAnomalySubscription =
      DeleteAnomalySubscriptionResponse
  request = postJSON costExplorer
  response =
    receiveEmpty
      ( \s h x ->
          DeleteAnomalySubscriptionResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteAnomalySubscription

instance NFData DeleteAnomalySubscription

instance ToHeaders DeleteAnomalySubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.DeleteAnomalySubscription" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAnomalySubscription where
  toJSON DeleteAnomalySubscription' {..} =
    object
      (catMaybes [Just ("SubscriptionArn" .= _dasSubscriptionARN)])

instance ToPath DeleteAnomalySubscription where
  toPath = const "/"

instance ToQuery DeleteAnomalySubscription where
  toQuery = const mempty

-- | /See:/ 'deleteAnomalySubscriptionResponse' smart constructor.
newtype DeleteAnomalySubscriptionResponse = DeleteAnomalySubscriptionResponse'
  { _dasrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAnomalySubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsResponseStatus' - -- | The response status code.
deleteAnomalySubscriptionResponse ::
  -- | 'dasrsResponseStatus'
  Int ->
  DeleteAnomalySubscriptionResponse
deleteAnomalySubscriptionResponse pResponseStatus_ =
  DeleteAnomalySubscriptionResponse'
    { _dasrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DeleteAnomalySubscriptionResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\s a -> s {_dasrsResponseStatus = a})

instance NFData DeleteAnomalySubscriptionResponse
