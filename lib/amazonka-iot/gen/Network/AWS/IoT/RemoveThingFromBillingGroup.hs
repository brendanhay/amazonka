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
-- Module      : Network.AWS.IoT.RemoveThingFromBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the given thing from the billing group.
module Network.AWS.IoT.RemoveThingFromBillingGroup
  ( -- * Creating a Request
    removeThingFromBillingGroup,
    RemoveThingFromBillingGroup,

    -- * Request Lenses
    rtfbgThingARN,
    rtfbgBillingGroupARN,
    rtfbgThingName,
    rtfbgBillingGroupName,

    -- * Destructuring the Response
    removeThingFromBillingGroupResponse,
    RemoveThingFromBillingGroupResponse,

    -- * Response Lenses
    rtfbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeThingFromBillingGroup' smart constructor.
data RemoveThingFromBillingGroup = RemoveThingFromBillingGroup'
  { _rtfbgThingARN ::
      !(Maybe Text),
    _rtfbgBillingGroupARN ::
      !(Maybe Text),
    _rtfbgThingName :: !(Maybe Text),
    _rtfbgBillingGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveThingFromBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfbgThingARN' - The ARN of the thing to be removed from the billing group.
--
-- * 'rtfbgBillingGroupARN' - The ARN of the billing group.
--
-- * 'rtfbgThingName' - The name of the thing to be removed from the billing group.
--
-- * 'rtfbgBillingGroupName' - The name of the billing group.
removeThingFromBillingGroup ::
  RemoveThingFromBillingGroup
removeThingFromBillingGroup =
  RemoveThingFromBillingGroup'
    { _rtfbgThingARN = Nothing,
      _rtfbgBillingGroupARN = Nothing,
      _rtfbgThingName = Nothing,
      _rtfbgBillingGroupName = Nothing
    }

-- | The ARN of the thing to be removed from the billing group.
rtfbgThingARN :: Lens' RemoveThingFromBillingGroup (Maybe Text)
rtfbgThingARN = lens _rtfbgThingARN (\s a -> s {_rtfbgThingARN = a})

-- | The ARN of the billing group.
rtfbgBillingGroupARN :: Lens' RemoveThingFromBillingGroup (Maybe Text)
rtfbgBillingGroupARN = lens _rtfbgBillingGroupARN (\s a -> s {_rtfbgBillingGroupARN = a})

-- | The name of the thing to be removed from the billing group.
rtfbgThingName :: Lens' RemoveThingFromBillingGroup (Maybe Text)
rtfbgThingName = lens _rtfbgThingName (\s a -> s {_rtfbgThingName = a})

-- | The name of the billing group.
rtfbgBillingGroupName :: Lens' RemoveThingFromBillingGroup (Maybe Text)
rtfbgBillingGroupName = lens _rtfbgBillingGroupName (\s a -> s {_rtfbgBillingGroupName = a})

instance AWSRequest RemoveThingFromBillingGroup where
  type
    Rs RemoveThingFromBillingGroup =
      RemoveThingFromBillingGroupResponse
  request = putJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          RemoveThingFromBillingGroupResponse' <$> (pure (fromEnum s))
      )

instance Hashable RemoveThingFromBillingGroup

instance NFData RemoveThingFromBillingGroup

instance ToHeaders RemoveThingFromBillingGroup where
  toHeaders = const mempty

instance ToJSON RemoveThingFromBillingGroup where
  toJSON RemoveThingFromBillingGroup' {..} =
    object
      ( catMaybes
          [ ("thingArn" .=) <$> _rtfbgThingARN,
            ("billingGroupArn" .=) <$> _rtfbgBillingGroupARN,
            ("thingName" .=) <$> _rtfbgThingName,
            ("billingGroupName" .=) <$> _rtfbgBillingGroupName
          ]
      )

instance ToPath RemoveThingFromBillingGroup where
  toPath = const "/billing-groups/removeThingFromBillingGroup"

instance ToQuery RemoveThingFromBillingGroup where
  toQuery = const mempty

-- | /See:/ 'removeThingFromBillingGroupResponse' smart constructor.
newtype RemoveThingFromBillingGroupResponse = RemoveThingFromBillingGroupResponse'
  { _rtfbgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveThingFromBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtfbgrsResponseStatus' - -- | The response status code.
removeThingFromBillingGroupResponse ::
  -- | 'rtfbgrsResponseStatus'
  Int ->
  RemoveThingFromBillingGroupResponse
removeThingFromBillingGroupResponse pResponseStatus_ =
  RemoveThingFromBillingGroupResponse'
    { _rtfbgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
rtfbgrsResponseStatus :: Lens' RemoveThingFromBillingGroupResponse Int
rtfbgrsResponseStatus = lens _rtfbgrsResponseStatus (\s a -> s {_rtfbgrsResponseStatus = a})

instance NFData RemoveThingFromBillingGroupResponse
