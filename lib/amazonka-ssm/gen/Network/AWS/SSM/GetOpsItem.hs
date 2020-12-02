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
-- Module      : Network.AWS.SSM.GetOpsItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an OpsItem by using the ID. You must have permission in AWS Identity and Access Management (IAM) to view information about an OpsItem. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter-getting-started.html Getting started with OpsCenter> in the /AWS Systems Manager User Guide/ .
--
--
-- Operations engineers and IT professionals use OpsCenter to view, investigate, and remediate operational issues impacting the performance and health of their AWS resources. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/OpsCenter.html AWS Systems Manager OpsCenter> in the /AWS Systems Manager User Guide/ .
module Network.AWS.SSM.GetOpsItem
  ( -- * Creating a Request
    getOpsItem,
    GetOpsItem,

    -- * Request Lenses
    goiOpsItemId,

    -- * Destructuring the Response
    getOpsItemResponse,
    GetOpsItemResponse,

    -- * Response Lenses
    goirsOpsItem,
    goirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'getOpsItem' smart constructor.
newtype GetOpsItem = GetOpsItem' {_goiOpsItemId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOpsItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goiOpsItemId' - The ID of the OpsItem that you want to get.
getOpsItem ::
  -- | 'goiOpsItemId'
  Text ->
  GetOpsItem
getOpsItem pOpsItemId_ = GetOpsItem' {_goiOpsItemId = pOpsItemId_}

-- | The ID of the OpsItem that you want to get.
goiOpsItemId :: Lens' GetOpsItem Text
goiOpsItemId = lens _goiOpsItemId (\s a -> s {_goiOpsItemId = a})

instance AWSRequest GetOpsItem where
  type Rs GetOpsItem = GetOpsItemResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetOpsItemResponse' <$> (x .?> "OpsItem") <*> (pure (fromEnum s))
      )

instance Hashable GetOpsItem

instance NFData GetOpsItem

instance ToHeaders GetOpsItem where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetOpsItem" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetOpsItem where
  toJSON GetOpsItem' {..} =
    object (catMaybes [Just ("OpsItemId" .= _goiOpsItemId)])

instance ToPath GetOpsItem where
  toPath = const "/"

instance ToQuery GetOpsItem where
  toQuery = const mempty

-- | /See:/ 'getOpsItemResponse' smart constructor.
data GetOpsItemResponse = GetOpsItemResponse'
  { _goirsOpsItem ::
      !(Maybe OpsItem),
    _goirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOpsItemResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goirsOpsItem' - The OpsItem.
--
-- * 'goirsResponseStatus' - -- | The response status code.
getOpsItemResponse ::
  -- | 'goirsResponseStatus'
  Int ->
  GetOpsItemResponse
getOpsItemResponse pResponseStatus_ =
  GetOpsItemResponse'
    { _goirsOpsItem = Nothing,
      _goirsResponseStatus = pResponseStatus_
    }

-- | The OpsItem.
goirsOpsItem :: Lens' GetOpsItemResponse (Maybe OpsItem)
goirsOpsItem = lens _goirsOpsItem (\s a -> s {_goirsOpsItem = a})

-- | -- | The response status code.
goirsResponseStatus :: Lens' GetOpsItemResponse Int
goirsResponseStatus = lens _goirsResponseStatus (\s a -> s {_goirsResponseStatus = a})

instance NFData GetOpsItemResponse
