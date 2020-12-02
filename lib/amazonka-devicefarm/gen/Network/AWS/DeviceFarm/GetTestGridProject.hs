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
-- Module      : Network.AWS.DeviceFarm.GetTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Selenium testing project.
module Network.AWS.DeviceFarm.GetTestGridProject
  ( -- * Creating a Request
    getTestGridProject,
    GetTestGridProject,

    -- * Request Lenses
    gtgpProjectARN,

    -- * Destructuring the Response
    getTestGridProjectResponse,
    GetTestGridProjectResponse,

    -- * Response Lenses
    gtgprsTestGridProject,
    gtgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTestGridProject' smart constructor.
newtype GetTestGridProject = GetTestGridProject'
  { _gtgpProjectARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTestGridProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgpProjectARN' - The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
getTestGridProject ::
  -- | 'gtgpProjectARN'
  Text ->
  GetTestGridProject
getTestGridProject pProjectARN_ =
  GetTestGridProject' {_gtgpProjectARN = pProjectARN_}

-- | The ARN of the Selenium testing project, from either 'CreateTestGridProject' or 'ListTestGridProjects' .
gtgpProjectARN :: Lens' GetTestGridProject Text
gtgpProjectARN = lens _gtgpProjectARN (\s a -> s {_gtgpProjectARN = a})

instance AWSRequest GetTestGridProject where
  type Rs GetTestGridProject = GetTestGridProjectResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          GetTestGridProjectResponse'
            <$> (x .?> "testGridProject") <*> (pure (fromEnum s))
      )

instance Hashable GetTestGridProject

instance NFData GetTestGridProject

instance ToHeaders GetTestGridProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.GetTestGridProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetTestGridProject where
  toJSON GetTestGridProject' {..} =
    object (catMaybes [Just ("projectArn" .= _gtgpProjectARN)])

instance ToPath GetTestGridProject where
  toPath = const "/"

instance ToQuery GetTestGridProject where
  toQuery = const mempty

-- | /See:/ 'getTestGridProjectResponse' smart constructor.
data GetTestGridProjectResponse = GetTestGridProjectResponse'
  { _gtgprsTestGridProject ::
      !(Maybe TestGridProject),
    _gtgprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTestGridProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgprsTestGridProject' - A 'TestGridProject' .
--
-- * 'gtgprsResponseStatus' - -- | The response status code.
getTestGridProjectResponse ::
  -- | 'gtgprsResponseStatus'
  Int ->
  GetTestGridProjectResponse
getTestGridProjectResponse pResponseStatus_ =
  GetTestGridProjectResponse'
    { _gtgprsTestGridProject = Nothing,
      _gtgprsResponseStatus = pResponseStatus_
    }

-- | A 'TestGridProject' .
gtgprsTestGridProject :: Lens' GetTestGridProjectResponse (Maybe TestGridProject)
gtgprsTestGridProject = lens _gtgprsTestGridProject (\s a -> s {_gtgprsTestGridProject = a})

-- | -- | The response status code.
gtgprsResponseStatus :: Lens' GetTestGridProjectResponse Int
gtgprsResponseStatus = lens _gtgprsResponseStatus (\s a -> s {_gtgprsResponseStatus = a})

instance NFData GetTestGridProjectResponse
