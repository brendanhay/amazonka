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
-- Module      : Network.AWS.DeviceFarm.UpdateTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change details of a project.
module Network.AWS.DeviceFarm.UpdateTestGridProject
  ( -- * Creating a Request
    updateTestGridProject,
    UpdateTestGridProject,

    -- * Request Lenses
    utgpName,
    utgpDescription,
    utgpProjectARN,

    -- * Destructuring the Response
    updateTestGridProjectResponse,
    UpdateTestGridProjectResponse,

    -- * Response Lenses
    utgprsTestGridProject,
    utgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTestGridProject' smart constructor.
data UpdateTestGridProject = UpdateTestGridProject'
  { _utgpName ::
      !(Maybe Text),
    _utgpDescription :: !(Maybe Text),
    _utgpProjectARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTestGridProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgpName' - Human-readable name for the project.
--
-- * 'utgpDescription' - Human-readable description for the project.
--
-- * 'utgpProjectARN' - ARN of the project to update.
updateTestGridProject ::
  -- | 'utgpProjectARN'
  Text ->
  UpdateTestGridProject
updateTestGridProject pProjectARN_ =
  UpdateTestGridProject'
    { _utgpName = Nothing,
      _utgpDescription = Nothing,
      _utgpProjectARN = pProjectARN_
    }

-- | Human-readable name for the project.
utgpName :: Lens' UpdateTestGridProject (Maybe Text)
utgpName = lens _utgpName (\s a -> s {_utgpName = a})

-- | Human-readable description for the project.
utgpDescription :: Lens' UpdateTestGridProject (Maybe Text)
utgpDescription = lens _utgpDescription (\s a -> s {_utgpDescription = a})

-- | ARN of the project to update.
utgpProjectARN :: Lens' UpdateTestGridProject Text
utgpProjectARN = lens _utgpProjectARN (\s a -> s {_utgpProjectARN = a})

instance AWSRequest UpdateTestGridProject where
  type Rs UpdateTestGridProject = UpdateTestGridProjectResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          UpdateTestGridProjectResponse'
            <$> (x .?> "testGridProject") <*> (pure (fromEnum s))
      )

instance Hashable UpdateTestGridProject

instance NFData UpdateTestGridProject

instance ToHeaders UpdateTestGridProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.UpdateTestGridProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateTestGridProject where
  toJSON UpdateTestGridProject' {..} =
    object
      ( catMaybes
          [ ("name" .=) <$> _utgpName,
            ("description" .=) <$> _utgpDescription,
            Just ("projectArn" .= _utgpProjectARN)
          ]
      )

instance ToPath UpdateTestGridProject where
  toPath = const "/"

instance ToQuery UpdateTestGridProject where
  toQuery = const mempty

-- | /See:/ 'updateTestGridProjectResponse' smart constructor.
data UpdateTestGridProjectResponse = UpdateTestGridProjectResponse'
  { _utgprsTestGridProject ::
      !(Maybe TestGridProject),
    _utgprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTestGridProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utgprsTestGridProject' - The project, including updated information.
--
-- * 'utgprsResponseStatus' - -- | The response status code.
updateTestGridProjectResponse ::
  -- | 'utgprsResponseStatus'
  Int ->
  UpdateTestGridProjectResponse
updateTestGridProjectResponse pResponseStatus_ =
  UpdateTestGridProjectResponse'
    { _utgprsTestGridProject = Nothing,
      _utgprsResponseStatus = pResponseStatus_
    }

-- | The project, including updated information.
utgprsTestGridProject :: Lens' UpdateTestGridProjectResponse (Maybe TestGridProject)
utgprsTestGridProject = lens _utgprsTestGridProject (\s a -> s {_utgprsTestGridProject = a})

-- | -- | The response status code.
utgprsResponseStatus :: Lens' UpdateTestGridProjectResponse Int
utgprsResponseStatus = lens _utgprsResponseStatus (\s a -> s {_utgprsResponseStatus = a})

instance NFData UpdateTestGridProjectResponse
