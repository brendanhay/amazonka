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
-- Module      : Network.AWS.DeviceFarm.CreateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Selenium testing project. Projects are used to track 'TestGridSession' instances.
module Network.AWS.DeviceFarm.CreateTestGridProject
  ( -- * Creating a Request
    createTestGridProject,
    CreateTestGridProject,

    -- * Request Lenses
    ctgpDescription,
    ctgpName,

    -- * Destructuring the Response
    createTestGridProjectResponse,
    CreateTestGridProjectResponse,

    -- * Response Lenses
    ctgprsTestGridProject,
    ctgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { _ctgpDescription ::
      !(Maybe Text),
    _ctgpName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTestGridProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgpDescription' - Human-readable description of the project.
--
-- * 'ctgpName' - Human-readable name of the Selenium testing project.
createTestGridProject ::
  -- | 'ctgpName'
  Text ->
  CreateTestGridProject
createTestGridProject pName_ =
  CreateTestGridProject'
    { _ctgpDescription = Nothing,
      _ctgpName = pName_
    }

-- | Human-readable description of the project.
ctgpDescription :: Lens' CreateTestGridProject (Maybe Text)
ctgpDescription = lens _ctgpDescription (\s a -> s {_ctgpDescription = a})

-- | Human-readable name of the Selenium testing project.
ctgpName :: Lens' CreateTestGridProject Text
ctgpName = lens _ctgpName (\s a -> s {_ctgpName = a})

instance AWSRequest CreateTestGridProject where
  type Rs CreateTestGridProject = CreateTestGridProjectResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          CreateTestGridProjectResponse'
            <$> (x .?> "testGridProject") <*> (pure (fromEnum s))
      )

instance Hashable CreateTestGridProject

instance NFData CreateTestGridProject

instance ToHeaders CreateTestGridProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.CreateTestGridProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTestGridProject where
  toJSON CreateTestGridProject' {..} =
    object
      ( catMaybes
          [ ("description" .=) <$> _ctgpDescription,
            Just ("name" .= _ctgpName)
          ]
      )

instance ToPath CreateTestGridProject where
  toPath = const "/"

instance ToQuery CreateTestGridProject where
  toQuery = const mempty

-- | /See:/ 'createTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
  { _ctgprsTestGridProject ::
      !(Maybe TestGridProject),
    _ctgprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTestGridProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctgprsTestGridProject' - ARN of the Selenium testing project that was created.
--
-- * 'ctgprsResponseStatus' - -- | The response status code.
createTestGridProjectResponse ::
  -- | 'ctgprsResponseStatus'
  Int ->
  CreateTestGridProjectResponse
createTestGridProjectResponse pResponseStatus_ =
  CreateTestGridProjectResponse'
    { _ctgprsTestGridProject = Nothing,
      _ctgprsResponseStatus = pResponseStatus_
    }

-- | ARN of the Selenium testing project that was created.
ctgprsTestGridProject :: Lens' CreateTestGridProjectResponse (Maybe TestGridProject)
ctgprsTestGridProject = lens _ctgprsTestGridProject (\s a -> s {_ctgprsTestGridProject = a})

-- | -- | The response status code.
ctgprsResponseStatus :: Lens' CreateTestGridProjectResponse Int
ctgprsResponseStatus = lens _ctgprsResponseStatus (\s a -> s {_ctgprsResponseStatus = a})

instance NFData CreateTestGridProjectResponse
