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
-- Module      : Network.AWS.DeviceFarm.DeleteTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Selenium testing project and all content generated under it.
--
--
-- /Important:/ You cannot undo this operation.
module Network.AWS.DeviceFarm.DeleteTestGridProject
  ( -- * Creating a Request
    deleteTestGridProject,
    DeleteTestGridProject,

    -- * Request Lenses
    dtgpProjectARN,

    -- * Destructuring the Response
    deleteTestGridProjectResponse,
    DeleteTestGridProjectResponse,

    -- * Response Lenses
    dtgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTestGridProject' smart constructor.
newtype DeleteTestGridProject = DeleteTestGridProject'
  { _dtgpProjectARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTestGridProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgpProjectARN' - The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
deleteTestGridProject ::
  -- | 'dtgpProjectARN'
  Text ->
  DeleteTestGridProject
deleteTestGridProject pProjectARN_ =
  DeleteTestGridProject' {_dtgpProjectARN = pProjectARN_}

-- | The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
dtgpProjectARN :: Lens' DeleteTestGridProject Text
dtgpProjectARN = lens _dtgpProjectARN (\s a -> s {_dtgpProjectARN = a})

instance AWSRequest DeleteTestGridProject where
  type Rs DeleteTestGridProject = DeleteTestGridProjectResponse
  request = postJSON deviceFarm
  response =
    receiveEmpty
      (\s h x -> DeleteTestGridProjectResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTestGridProject

instance NFData DeleteTestGridProject

instance ToHeaders DeleteTestGridProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.DeleteTestGridProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteTestGridProject where
  toJSON DeleteTestGridProject' {..} =
    object (catMaybes [Just ("projectArn" .= _dtgpProjectARN)])

instance ToPath DeleteTestGridProject where
  toPath = const "/"

instance ToQuery DeleteTestGridProject where
  toQuery = const mempty

-- | /See:/ 'deleteTestGridProjectResponse' smart constructor.
newtype DeleteTestGridProjectResponse = DeleteTestGridProjectResponse'
  { _dtgprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTestGridProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgprsResponseStatus' - -- | The response status code.
deleteTestGridProjectResponse ::
  -- | 'dtgprsResponseStatus'
  Int ->
  DeleteTestGridProjectResponse
deleteTestGridProjectResponse pResponseStatus_ =
  DeleteTestGridProjectResponse'
    { _dtgprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dtgprsResponseStatus :: Lens' DeleteTestGridProjectResponse Int
dtgprsResponseStatus = lens _dtgprsResponseStatus (\s a -> s {_dtgprsResponseStatus = a})

instance NFData DeleteTestGridProjectResponse
