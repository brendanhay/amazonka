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
-- Module      : Network.AWS.EC2.DeleteQueuedReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the queued purchases for the specified Reserved Instances.
module Network.AWS.EC2.DeleteQueuedReservedInstances
  ( -- * Creating a Request
    deleteQueuedReservedInstances,
    DeleteQueuedReservedInstances,

    -- * Request Lenses
    dqriDryRun,
    dqriReservedInstancesIds,

    -- * Destructuring the Response
    deleteQueuedReservedInstancesResponse,
    DeleteQueuedReservedInstancesResponse,

    -- * Response Lenses
    dqrirsFailedQueuedPurchaseDeletions,
    dqrirsSuccessfulQueuedPurchaseDeletions,
    dqrirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteQueuedReservedInstances' smart constructor.
data DeleteQueuedReservedInstances = DeleteQueuedReservedInstances'
  { _dqriDryRun ::
      !(Maybe Bool),
    _dqriReservedInstancesIds ::
      !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteQueuedReservedInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqriDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dqriReservedInstancesIds' - The IDs of the Reserved Instances.
deleteQueuedReservedInstances ::
  -- | 'dqriReservedInstancesIds'
  NonEmpty Text ->
  DeleteQueuedReservedInstances
deleteQueuedReservedInstances pReservedInstancesIds_ =
  DeleteQueuedReservedInstances'
    { _dqriDryRun = Nothing,
      _dqriReservedInstancesIds = _List1 # pReservedInstancesIds_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dqriDryRun :: Lens' DeleteQueuedReservedInstances (Maybe Bool)
dqriDryRun = lens _dqriDryRun (\s a -> s {_dqriDryRun = a})

-- | The IDs of the Reserved Instances.
dqriReservedInstancesIds :: Lens' DeleteQueuedReservedInstances (NonEmpty Text)
dqriReservedInstancesIds = lens _dqriReservedInstancesIds (\s a -> s {_dqriReservedInstancesIds = a}) . _List1

instance AWSRequest DeleteQueuedReservedInstances where
  type
    Rs DeleteQueuedReservedInstances =
      DeleteQueuedReservedInstancesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteQueuedReservedInstancesResponse'
            <$> ( x .@? "failedQueuedPurchaseDeletionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> ( x .@? "successfulQueuedPurchaseDeletionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteQueuedReservedInstances

instance NFData DeleteQueuedReservedInstances

instance ToHeaders DeleteQueuedReservedInstances where
  toHeaders = const mempty

instance ToPath DeleteQueuedReservedInstances where
  toPath = const "/"

instance ToQuery DeleteQueuedReservedInstances where
  toQuery DeleteQueuedReservedInstances' {..} =
    mconcat
      [ "Action" =: ("DeleteQueuedReservedInstances" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dqriDryRun,
        toQueryList "ReservedInstancesId" _dqriReservedInstancesIds
      ]

-- | /See:/ 'deleteQueuedReservedInstancesResponse' smart constructor.
data DeleteQueuedReservedInstancesResponse = DeleteQueuedReservedInstancesResponse'
  { _dqrirsFailedQueuedPurchaseDeletions ::
      !( Maybe
           [FailedQueuedPurchaseDeletion]
       ),
    _dqrirsSuccessfulQueuedPurchaseDeletions ::
      !( Maybe
           [SuccessfulQueuedPurchaseDeletion]
       ),
    _dqrirsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteQueuedReservedInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqrirsFailedQueuedPurchaseDeletions' - Information about the queued purchases that could not be deleted.
--
-- * 'dqrirsSuccessfulQueuedPurchaseDeletions' - Information about the queued purchases that were successfully deleted.
--
-- * 'dqrirsResponseStatus' - -- | The response status code.
deleteQueuedReservedInstancesResponse ::
  -- | 'dqrirsResponseStatus'
  Int ->
  DeleteQueuedReservedInstancesResponse
deleteQueuedReservedInstancesResponse pResponseStatus_ =
  DeleteQueuedReservedInstancesResponse'
    { _dqrirsFailedQueuedPurchaseDeletions =
        Nothing,
      _dqrirsSuccessfulQueuedPurchaseDeletions = Nothing,
      _dqrirsResponseStatus = pResponseStatus_
    }

-- | Information about the queued purchases that could not be deleted.
dqrirsFailedQueuedPurchaseDeletions :: Lens' DeleteQueuedReservedInstancesResponse [FailedQueuedPurchaseDeletion]
dqrirsFailedQueuedPurchaseDeletions = lens _dqrirsFailedQueuedPurchaseDeletions (\s a -> s {_dqrirsFailedQueuedPurchaseDeletions = a}) . _Default . _Coerce

-- | Information about the queued purchases that were successfully deleted.
dqrirsSuccessfulQueuedPurchaseDeletions :: Lens' DeleteQueuedReservedInstancesResponse [SuccessfulQueuedPurchaseDeletion]
dqrirsSuccessfulQueuedPurchaseDeletions = lens _dqrirsSuccessfulQueuedPurchaseDeletions (\s a -> s {_dqrirsSuccessfulQueuedPurchaseDeletions = a}) . _Default . _Coerce

-- | -- | The response status code.
dqrirsResponseStatus :: Lens' DeleteQueuedReservedInstancesResponse Int
dqrirsResponseStatus = lens _dqrirsResponseStatus (\s a -> s {_dqrirsResponseStatus = a})

instance NFData DeleteQueuedReservedInstancesResponse
