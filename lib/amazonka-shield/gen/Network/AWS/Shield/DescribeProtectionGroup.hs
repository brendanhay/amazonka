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
-- Module      : Network.AWS.Shield.DescribeProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specification for the specified protection group.
module Network.AWS.Shield.DescribeProtectionGroup
  ( -- * Creating a Request
    describeProtectionGroup,
    DescribeProtectionGroup,

    -- * Request Lenses
    dProtectionGroupId,

    -- * Destructuring the Response
    describeProtectionGroupResponse,
    DescribeProtectionGroupResponse,

    -- * Response Lenses
    drsResponseStatus,
    drsProtectionGroup,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'describeProtectionGroup' smart constructor.
newtype DescribeProtectionGroup = DescribeProtectionGroup'
  { _dProtectionGroupId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
describeProtectionGroup ::
  -- | 'dProtectionGroupId'
  Text ->
  DescribeProtectionGroup
describeProtectionGroup pProtectionGroupId_ =
  DescribeProtectionGroup'
    { _dProtectionGroupId =
        pProtectionGroupId_
    }

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
dProtectionGroupId :: Lens' DescribeProtectionGroup Text
dProtectionGroupId = lens _dProtectionGroupId (\s a -> s {_dProtectionGroupId = a})

instance AWSRequest DescribeProtectionGroup where
  type Rs DescribeProtectionGroup = DescribeProtectionGroupResponse
  request = postJSON shield
  response =
    receiveJSON
      ( \s h x ->
          DescribeProtectionGroupResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "ProtectionGroup")
      )

instance Hashable DescribeProtectionGroup

instance NFData DescribeProtectionGroup

instance ToHeaders DescribeProtectionGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.DescribeProtectionGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeProtectionGroup where
  toJSON DescribeProtectionGroup' {..} =
    object
      (catMaybes [Just ("ProtectionGroupId" .= _dProtectionGroupId)])

instance ToPath DescribeProtectionGroup where
  toPath = const "/"

instance ToQuery DescribeProtectionGroup where
  toQuery = const mempty

-- | /See:/ 'describeProtectionGroupResponse' smart constructor.
data DescribeProtectionGroupResponse = DescribeProtectionGroupResponse'
  { _drsResponseStatus ::
      !Int,
    _drsProtectionGroup ::
      !ProtectionGroup
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProtectionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
--
-- * 'drsProtectionGroup' - A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
describeProtectionGroupResponse ::
  -- | 'drsResponseStatus'
  Int ->
  -- | 'drsProtectionGroup'
  ProtectionGroup ->
  DescribeProtectionGroupResponse
describeProtectionGroupResponse pResponseStatus_ pProtectionGroup_ =
  DescribeProtectionGroupResponse'
    { _drsResponseStatus =
        pResponseStatus_,
      _drsProtectionGroup = pProtectionGroup_
    }

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeProtectionGroupResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
drsProtectionGroup :: Lens' DescribeProtectionGroupResponse ProtectionGroup
drsProtectionGroup = lens _drsProtectionGroup (\s a -> s {_drsProtectionGroup = a})

instance NFData DescribeProtectionGroupResponse
