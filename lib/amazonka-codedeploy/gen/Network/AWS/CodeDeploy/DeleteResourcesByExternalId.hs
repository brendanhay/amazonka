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
-- Module      : Network.AWS.CodeDeploy.DeleteResourcesByExternalId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes resources linked to an external ID.
module Network.AWS.CodeDeploy.DeleteResourcesByExternalId
  ( -- * Creating a Request
    deleteResourcesByExternalId,
    DeleteResourcesByExternalId,

    -- * Request Lenses
    drbeiExternalId,

    -- * Destructuring the Response
    deleteResourcesByExternalIdResponse,
    DeleteResourcesByExternalIdResponse,

    -- * Response Lenses
    drbeirsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourcesByExternalId' smart constructor.
newtype DeleteResourcesByExternalId = DeleteResourcesByExternalId'
  { _drbeiExternalId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourcesByExternalId' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drbeiExternalId' - The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
deleteResourcesByExternalId ::
  DeleteResourcesByExternalId
deleteResourcesByExternalId =
  DeleteResourcesByExternalId' {_drbeiExternalId = Nothing}

-- | The unique ID of an external resource (for example, a CloudFormation stack ID) that is linked to one or more CodeDeploy resources.
drbeiExternalId :: Lens' DeleteResourcesByExternalId (Maybe Text)
drbeiExternalId = lens _drbeiExternalId (\s a -> s {_drbeiExternalId = a})

instance AWSRequest DeleteResourcesByExternalId where
  type
    Rs DeleteResourcesByExternalId =
      DeleteResourcesByExternalIdResponse
  request = postJSON codeDeploy
  response =
    receiveEmpty
      ( \s h x ->
          DeleteResourcesByExternalIdResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteResourcesByExternalId

instance NFData DeleteResourcesByExternalId

instance ToHeaders DeleteResourcesByExternalId where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeDeploy_20141006.DeleteResourcesByExternalId" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteResourcesByExternalId where
  toJSON DeleteResourcesByExternalId' {..} =
    object (catMaybes [("externalId" .=) <$> _drbeiExternalId])

instance ToPath DeleteResourcesByExternalId where
  toPath = const "/"

instance ToQuery DeleteResourcesByExternalId where
  toQuery = const mempty

-- | /See:/ 'deleteResourcesByExternalIdResponse' smart constructor.
newtype DeleteResourcesByExternalIdResponse = DeleteResourcesByExternalIdResponse'
  { _drbeirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourcesByExternalIdResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drbeirsResponseStatus' - -- | The response status code.
deleteResourcesByExternalIdResponse ::
  -- | 'drbeirsResponseStatus'
  Int ->
  DeleteResourcesByExternalIdResponse
deleteResourcesByExternalIdResponse pResponseStatus_ =
  DeleteResourcesByExternalIdResponse'
    { _drbeirsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drbeirsResponseStatus :: Lens' DeleteResourcesByExternalIdResponse Int
drbeirsResponseStatus = lens _drbeirsResponseStatus (\s a -> s {_drbeirsResponseStatus = a})

instance NFData DeleteResourcesByExternalIdResponse
