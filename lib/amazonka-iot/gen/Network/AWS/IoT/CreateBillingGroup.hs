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
-- Module      : Network.AWS.IoT.CreateBillingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a billing group.
module Network.AWS.IoT.CreateBillingGroup
  ( -- * Creating a Request
    createBillingGroup,
    CreateBillingGroup,

    -- * Request Lenses
    cbgBillingGroupProperties,
    cbgTags,
    cbgBillingGroupName,

    -- * Destructuring the Response
    createBillingGroupResponse,
    CreateBillingGroupResponse,

    -- * Response Lenses
    cbgrsBillingGroupARN,
    cbgrsBillingGroupName,
    cbgrsBillingGroupId,
    cbgrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createBillingGroup' smart constructor.
data CreateBillingGroup = CreateBillingGroup'
  { _cbgBillingGroupProperties ::
      !(Maybe BillingGroupProperties),
    _cbgTags :: !(Maybe [Tag]),
    _cbgBillingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBillingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbgBillingGroupProperties' - The properties of the billing group.
--
-- * 'cbgTags' - Metadata which can be used to manage the billing group.
--
-- * 'cbgBillingGroupName' - The name you wish to give to the billing group.
createBillingGroup ::
  -- | 'cbgBillingGroupName'
  Text ->
  CreateBillingGroup
createBillingGroup pBillingGroupName_ =
  CreateBillingGroup'
    { _cbgBillingGroupProperties = Nothing,
      _cbgTags = Nothing,
      _cbgBillingGroupName = pBillingGroupName_
    }

-- | The properties of the billing group.
cbgBillingGroupProperties :: Lens' CreateBillingGroup (Maybe BillingGroupProperties)
cbgBillingGroupProperties = lens _cbgBillingGroupProperties (\s a -> s {_cbgBillingGroupProperties = a})

-- | Metadata which can be used to manage the billing group.
cbgTags :: Lens' CreateBillingGroup [Tag]
cbgTags = lens _cbgTags (\s a -> s {_cbgTags = a}) . _Default . _Coerce

-- | The name you wish to give to the billing group.
cbgBillingGroupName :: Lens' CreateBillingGroup Text
cbgBillingGroupName = lens _cbgBillingGroupName (\s a -> s {_cbgBillingGroupName = a})

instance AWSRequest CreateBillingGroup where
  type Rs CreateBillingGroup = CreateBillingGroupResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateBillingGroupResponse'
            <$> (x .?> "billingGroupArn")
            <*> (x .?> "billingGroupName")
            <*> (x .?> "billingGroupId")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateBillingGroup

instance NFData CreateBillingGroup

instance ToHeaders CreateBillingGroup where
  toHeaders = const mempty

instance ToJSON CreateBillingGroup where
  toJSON CreateBillingGroup' {..} =
    object
      ( catMaybes
          [ ("billingGroupProperties" .=) <$> _cbgBillingGroupProperties,
            ("tags" .=) <$> _cbgTags
          ]
      )

instance ToPath CreateBillingGroup where
  toPath CreateBillingGroup' {..} =
    mconcat ["/billing-groups/", toBS _cbgBillingGroupName]

instance ToQuery CreateBillingGroup where
  toQuery = const mempty

-- | /See:/ 'createBillingGroupResponse' smart constructor.
data CreateBillingGroupResponse = CreateBillingGroupResponse'
  { _cbgrsBillingGroupARN ::
      !(Maybe Text),
    _cbgrsBillingGroupName ::
      !(Maybe Text),
    _cbgrsBillingGroupId :: !(Maybe Text),
    _cbgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBillingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbgrsBillingGroupARN' - The ARN of the billing group.
--
-- * 'cbgrsBillingGroupName' - The name you gave to the billing group.
--
-- * 'cbgrsBillingGroupId' - The ID of the billing group.
--
-- * 'cbgrsResponseStatus' - -- | The response status code.
createBillingGroupResponse ::
  -- | 'cbgrsResponseStatus'
  Int ->
  CreateBillingGroupResponse
createBillingGroupResponse pResponseStatus_ =
  CreateBillingGroupResponse'
    { _cbgrsBillingGroupARN = Nothing,
      _cbgrsBillingGroupName = Nothing,
      _cbgrsBillingGroupId = Nothing,
      _cbgrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the billing group.
cbgrsBillingGroupARN :: Lens' CreateBillingGroupResponse (Maybe Text)
cbgrsBillingGroupARN = lens _cbgrsBillingGroupARN (\s a -> s {_cbgrsBillingGroupARN = a})

-- | The name you gave to the billing group.
cbgrsBillingGroupName :: Lens' CreateBillingGroupResponse (Maybe Text)
cbgrsBillingGroupName = lens _cbgrsBillingGroupName (\s a -> s {_cbgrsBillingGroupName = a})

-- | The ID of the billing group.
cbgrsBillingGroupId :: Lens' CreateBillingGroupResponse (Maybe Text)
cbgrsBillingGroupId = lens _cbgrsBillingGroupId (\s a -> s {_cbgrsBillingGroupId = a})

-- | -- | The response status code.
cbgrsResponseStatus :: Lens' CreateBillingGroupResponse Int
cbgrsResponseStatus = lens _cbgrsResponseStatus (\s a -> s {_cbgrsResponseStatus = a})

instance NFData CreateBillingGroupResponse
