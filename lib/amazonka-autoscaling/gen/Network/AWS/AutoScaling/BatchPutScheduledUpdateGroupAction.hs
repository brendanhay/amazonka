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
-- Module      : Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates one or more scheduled scaling actions for an Auto Scaling group. If you leave a parameter unspecified when updating a scheduled scaling action, the corresponding value remains unchanged.
module Network.AWS.AutoScaling.BatchPutScheduledUpdateGroupAction
  ( -- * Creating a Request
    batchPutScheduledUpdateGroupAction,
    BatchPutScheduledUpdateGroupAction,

    -- * Request Lenses
    bpsugaAutoScalingGroupName,
    bpsugaScheduledUpdateGroupActions,

    -- * Destructuring the Response
    batchPutScheduledUpdateGroupActionResponse,
    BatchPutScheduledUpdateGroupActionResponse,

    -- * Response Lenses
    bpsugarsFailedScheduledUpdateGroupActions,
    bpsugarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchPutScheduledUpdateGroupAction' smart constructor.
data BatchPutScheduledUpdateGroupAction = BatchPutScheduledUpdateGroupAction'
  { _bpsugaAutoScalingGroupName ::
      !Text,
    _bpsugaScheduledUpdateGroupActions ::
      ![ScheduledUpdateGroupActionRequest]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchPutScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpsugaAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'bpsugaScheduledUpdateGroupActions' - One or more scheduled actions. The maximum number allowed is 50.
batchPutScheduledUpdateGroupAction ::
  -- | 'bpsugaAutoScalingGroupName'
  Text ->
  BatchPutScheduledUpdateGroupAction
batchPutScheduledUpdateGroupAction pAutoScalingGroupName_ =
  BatchPutScheduledUpdateGroupAction'
    { _bpsugaAutoScalingGroupName =
        pAutoScalingGroupName_,
      _bpsugaScheduledUpdateGroupActions = mempty
    }

-- | The name of the Auto Scaling group.
bpsugaAutoScalingGroupName :: Lens' BatchPutScheduledUpdateGroupAction Text
bpsugaAutoScalingGroupName = lens _bpsugaAutoScalingGroupName (\s a -> s {_bpsugaAutoScalingGroupName = a})

-- | One or more scheduled actions. The maximum number allowed is 50.
bpsugaScheduledUpdateGroupActions :: Lens' BatchPutScheduledUpdateGroupAction [ScheduledUpdateGroupActionRequest]
bpsugaScheduledUpdateGroupActions = lens _bpsugaScheduledUpdateGroupActions (\s a -> s {_bpsugaScheduledUpdateGroupActions = a}) . _Coerce

instance AWSRequest BatchPutScheduledUpdateGroupAction where
  type
    Rs BatchPutScheduledUpdateGroupAction =
      BatchPutScheduledUpdateGroupActionResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "BatchPutScheduledUpdateGroupActionResult"
      ( \s h x ->
          BatchPutScheduledUpdateGroupActionResponse'
            <$> ( x .@? "FailedScheduledUpdateGroupActions" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable BatchPutScheduledUpdateGroupAction

instance NFData BatchPutScheduledUpdateGroupAction

instance ToHeaders BatchPutScheduledUpdateGroupAction where
  toHeaders = const mempty

instance ToPath BatchPutScheduledUpdateGroupAction where
  toPath = const "/"

instance ToQuery BatchPutScheduledUpdateGroupAction where
  toQuery BatchPutScheduledUpdateGroupAction' {..} =
    mconcat
      [ "Action" =: ("BatchPutScheduledUpdateGroupAction" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "AutoScalingGroupName" =: _bpsugaAutoScalingGroupName,
        "ScheduledUpdateGroupActions"
          =: toQueryList "member" _bpsugaScheduledUpdateGroupActions
      ]

-- | /See:/ 'batchPutScheduledUpdateGroupActionResponse' smart constructor.
data BatchPutScheduledUpdateGroupActionResponse = BatchPutScheduledUpdateGroupActionResponse'
  { _bpsugarsFailedScheduledUpdateGroupActions ::
      !( Maybe
           [FailedScheduledUpdateGroupActionRequest]
       ),
    _bpsugarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'BatchPutScheduledUpdateGroupActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpsugarsFailedScheduledUpdateGroupActions' - The names of the scheduled actions that could not be created or updated, including an error message.
--
-- * 'bpsugarsResponseStatus' - -- | The response status code.
batchPutScheduledUpdateGroupActionResponse ::
  -- | 'bpsugarsResponseStatus'
  Int ->
  BatchPutScheduledUpdateGroupActionResponse
batchPutScheduledUpdateGroupActionResponse pResponseStatus_ =
  BatchPutScheduledUpdateGroupActionResponse'
    { _bpsugarsFailedScheduledUpdateGroupActions =
        Nothing,
      _bpsugarsResponseStatus = pResponseStatus_
    }

-- | The names of the scheduled actions that could not be created or updated, including an error message.
bpsugarsFailedScheduledUpdateGroupActions :: Lens' BatchPutScheduledUpdateGroupActionResponse [FailedScheduledUpdateGroupActionRequest]
bpsugarsFailedScheduledUpdateGroupActions = lens _bpsugarsFailedScheduledUpdateGroupActions (\s a -> s {_bpsugarsFailedScheduledUpdateGroupActions = a}) . _Default . _Coerce

-- | -- | The response status code.
bpsugarsResponseStatus :: Lens' BatchPutScheduledUpdateGroupActionResponse Int
bpsugarsResponseStatus = lens _bpsugarsResponseStatus (\s a -> s {_bpsugarsResponseStatus = a})

instance NFData BatchPutScheduledUpdateGroupActionResponse
