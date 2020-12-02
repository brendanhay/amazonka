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
-- Module      : Network.AWS.AutoScaling.BatchDeleteScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more scheduled actions for the specified Auto Scaling group.
module Network.AWS.AutoScaling.BatchDeleteScheduledAction
  ( -- * Creating a Request
    batchDeleteScheduledAction,
    BatchDeleteScheduledAction,

    -- * Request Lenses
    bdsaAutoScalingGroupName,
    bdsaScheduledActionNames,

    -- * Destructuring the Response
    batchDeleteScheduledActionResponse,
    BatchDeleteScheduledActionResponse,

    -- * Response Lenses
    bdsarsFailedScheduledActions,
    bdsarsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteScheduledAction' smart constructor.
data BatchDeleteScheduledAction = BatchDeleteScheduledAction'
  { _bdsaAutoScalingGroupName ::
      !Text,
    _bdsaScheduledActionNames :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteScheduledAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsaAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'bdsaScheduledActionNames' - The names of the scheduled actions to delete. The maximum number allowed is 50.
batchDeleteScheduledAction ::
  -- | 'bdsaAutoScalingGroupName'
  Text ->
  BatchDeleteScheduledAction
batchDeleteScheduledAction pAutoScalingGroupName_ =
  BatchDeleteScheduledAction'
    { _bdsaAutoScalingGroupName =
        pAutoScalingGroupName_,
      _bdsaScheduledActionNames = mempty
    }

-- | The name of the Auto Scaling group.
bdsaAutoScalingGroupName :: Lens' BatchDeleteScheduledAction Text
bdsaAutoScalingGroupName = lens _bdsaAutoScalingGroupName (\s a -> s {_bdsaAutoScalingGroupName = a})

-- | The names of the scheduled actions to delete. The maximum number allowed is 50.
bdsaScheduledActionNames :: Lens' BatchDeleteScheduledAction [Text]
bdsaScheduledActionNames = lens _bdsaScheduledActionNames (\s a -> s {_bdsaScheduledActionNames = a}) . _Coerce

instance AWSRequest BatchDeleteScheduledAction where
  type
    Rs BatchDeleteScheduledAction =
      BatchDeleteScheduledActionResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "BatchDeleteScheduledActionResult"
      ( \s h x ->
          BatchDeleteScheduledActionResponse'
            <$> ( x .@? "FailedScheduledActions" .!@ mempty
                    >>= may (parseXMLList "member")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable BatchDeleteScheduledAction

instance NFData BatchDeleteScheduledAction

instance ToHeaders BatchDeleteScheduledAction where
  toHeaders = const mempty

instance ToPath BatchDeleteScheduledAction where
  toPath = const "/"

instance ToQuery BatchDeleteScheduledAction where
  toQuery BatchDeleteScheduledAction' {..} =
    mconcat
      [ "Action" =: ("BatchDeleteScheduledAction" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "AutoScalingGroupName" =: _bdsaAutoScalingGroupName,
        "ScheduledActionNames"
          =: toQueryList "member" _bdsaScheduledActionNames
      ]

-- | /See:/ 'batchDeleteScheduledActionResponse' smart constructor.
data BatchDeleteScheduledActionResponse = BatchDeleteScheduledActionResponse'
  { _bdsarsFailedScheduledActions ::
      !( Maybe
           [FailedScheduledUpdateGroupActionRequest]
       ),
    _bdsarsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteScheduledActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdsarsFailedScheduledActions' - The names of the scheduled actions that could not be deleted, including an error message.
--
-- * 'bdsarsResponseStatus' - -- | The response status code.
batchDeleteScheduledActionResponse ::
  -- | 'bdsarsResponseStatus'
  Int ->
  BatchDeleteScheduledActionResponse
batchDeleteScheduledActionResponse pResponseStatus_ =
  BatchDeleteScheduledActionResponse'
    { _bdsarsFailedScheduledActions =
        Nothing,
      _bdsarsResponseStatus = pResponseStatus_
    }

-- | The names of the scheduled actions that could not be deleted, including an error message.
bdsarsFailedScheduledActions :: Lens' BatchDeleteScheduledActionResponse [FailedScheduledUpdateGroupActionRequest]
bdsarsFailedScheduledActions = lens _bdsarsFailedScheduledActions (\s a -> s {_bdsarsFailedScheduledActions = a}) . _Default . _Coerce

-- | -- | The response status code.
bdsarsResponseStatus :: Lens' BatchDeleteScheduledActionResponse Int
bdsarsResponseStatus = lens _bdsarsResponseStatus (\s a -> s {_bdsarsResponseStatus = a})

instance NFData BatchDeleteScheduledActionResponse
